open Bap.Std
open Core_kernel


module NodeTemplate = struct
  type t =
    | BlkStart of Blk.t
    | BlkEnd of Blk.t
    | CallReturn of Blk.t (* The Blk.t is the block from the call instruction *)
  [@@deriving bin_io, compare, sexp, hash]
end

module Node = struct
  include NodeTemplate

  include Regular.Std.Opaque.Make(NodeTemplate)
end

module EdgeLabel = struct
  type t =
    | Block
    | Jump of Jmp.t * (Jmp.t Option.t) (* The second value is an untaken conditional jump instruction *)
    | Call of Call.t
    | ExternCallStub of Call.t
    | CRCallStub of Call.t
    | CRReturnStub of Blk.t
    | CRCombine
end

module Graph = Graphlib.Std.Graphlib.Make(Node)(EdgeLabel)

module GraphBuilder = struct
  (** This type contains the context and the intermediate state for the build process of the interprocedural fixpoint graph *)
  type t = {
    project: Project.t;
    graph: Graph.t;
    call_return_map: Blk.t List.t Sub.Map.t;
    call_worklist: (Call.t * Blk.t * Sub.t) List.t (* each element is a call, the block containing the call, the sub containing the block.*)
  }

  let empty (project: Project.t) : t =
    { project = project;
      graph = Graph.empty;
      call_return_map = Sub.Map.empty;
      call_worklist = []
    }

  (** add the block start and end nodes and the block edge to the graph. *)
  let add_block (builder: t) (block: Blk.t) : t =
    let graph = Graph.Node.insert (BlkStart block) builder.graph in
    let graph = Graph.Node.insert (BlkEnd block) graph in
    let edge = Graph.Edge.create (BlkStart block) (BlkEnd block) Block in
    { builder with graph = Graph.Edge.insert edge graph }

  (** add call and return edges corresponding to a call to the control flow graph *)
  let add_call_edges (builder: t) (call: Call.t) (source: Blk.t) ~(caller: Sub.t) : t =
    match Call.target call with
    | Direct target_tid ->
        let func_name = match String.lsplit2 (Tid.name target_tid) ~on:'@'  with
          | Some(_left, right) -> right
          | None -> Tid.name target_tid in
        if String.Set.mem (Cconv.parse_dyn_syms builder.project) func_name then
          (* This is an extern symbol. TODO: refactor this test to a function in symbol_utils *)
          match Call.return call with
          | Some (Direct return_tid) ->
              let return_block = Term.find_exn blk_t caller return_tid in
              let edge = Graph.Edge.create (BlkEnd source) (BlkStart return_block) (ExternCallStub call) in
              {builder with graph = Graph.Edge.insert edge builder.graph }
          | None ->
              builder (* a non-returning extern symbol gets no edge in the graph. TODO: Check that there are no conditional calls! Else I need a dummy edge here.*)
          | Some (Indirect _) ->
              failwith "Error: Function call with indirect return address encountered."
        else
          (* This is an internal call. *)
          let target_sub = Term.find_exn sub_t (Project.program builder.project) target_tid in
          let target_block = Option.value_exn ~message:"Error: Sub has no basic blocks" (Term.first blk_t target_sub) in
          let call_edge = Graph.Edge.create (BlkEnd source) (BlkStart target_block) (Call call) in
          let graph = Graph.Edge.insert call_edge builder.graph in
          let call_stub_edge = Graph.Edge.create (BlkEnd source) (CallReturn source) (CRCallStub call) in
          let graph = Graph.Edge.insert call_stub_edge graph in
          let return_list = Map.find_exn builder.call_return_map caller in
          let graph = List.fold return_list ~init:graph ~f:(fun graph return_block ->
            let return_stub_edge = Graph.Edge.create (BlkEnd return_block) (CallReturn source) (CRReturnStub return_block) in
            Graph.Edge.insert return_stub_edge graph
          ) in
          let combinator_edge = Graph.Edge.create (CallReturn source) (BlkStart target_block) CRCombine in
          {builder with graph = Graph.Edge.insert combinator_edge graph }
    | Indirect _ ->
        (* For the moment, indirect calls are dead ends in the control flow graph.
           How they will be handled in the future is not decided yet. *)
        builder

  (** Add all blocks and jumps from the subfunction to the graph.
      All calls get added to the call worklist.
      The list of all blocks of the function containing a return instruction gets added to the call_return_map. *)
  let add_subfunction_cfg (builder: t) (sub: Sub.t) : t =
    let module G = Graphs.Tid in
    let bap_cfg = Sub.to_graph sub in
    let call_list = ref [] in
    let return_list = ref [] in
    let block_tids = G.nodes bap_cfg in
    (* add all blocks to the graph *)
    let builder = Seq.fold block_tids ~init:builder ~f:(fun builder block_tid ->
      let block = Term.find_exn blk_t sub block_tid in
      let jumps = Term.enum jmp_t block in
      let () = Seq.iter jumps ~f:(fun jump ->
        match Jmp.kind jump with
        | Call call -> call_list := (call, block, sub) :: !call_list
        | Ret _ -> return_list := block :: !return_list
        | _ -> ()
      ) in
      add_block builder block
    ) in
    let edges = G.edges bap_cfg in
    (* add all edges stemming from intraprocedural jumps to the graph *)
    let graph = Seq.fold edges ~init:builder.graph ~f:(fun graph edge ->
      let source = Term.find_exn blk_t sub (G.Edge.src edge) in
      let destination = Term.find_exn blk_t sub (G.Edge.dst edge) in
      let jump_term = Term.find_exn jmp_t source (G.Edge.label edge) in
      let untaken_conditional_jump = match Seq.to_list (Term.enum jmp_t source) with (* TODO: write test case to check whether this works as intended *)
        | _ :: [] -> None
        | first_jmp :: _second_jmp :: [] ->
            if jump_term = first_jmp then
              None
            else (* TODO: add a check that first_jmp isn't an unconditional jump *)
              (Some first_jmp)
        | other -> failwith ("Error: Basic block has unexpected number of " ^ (Int.to_string (List.length other)) ^ " jump terms") in
      match Jmp.kind jump_term with
      | Goto _ ->
          let  new_edge = Graph.Edge.create (BlkEnd source) (BlkStart destination) (Jump (jump_term, untaken_conditional_jump)) in
          Graph.Edge.insert new_edge graph
      | Call _ | Ret _ | Int _ -> failwith "Error: Unexpected edge type in subfunction control flow graph"
    ) in
    { builder with graph = graph;
                   call_return_map = Map.set builder.call_return_map ~key:sub ~data:!return_list;
                   call_worklist = !call_list @ builder.call_worklist}

end

let generate_fixpoint_cfg (project: Project.t) : Graph.t =
  let builder = GraphBuilder.empty project in
  let functions = Term.enum sub_t (Project.program project) in
  let builder = Seq.fold functions ~init:builder ~f:(fun builder sub ->
    GraphBuilder.add_subfunction_cfg builder sub ) in
  let builder = List.fold builder.call_worklist ~init:builder ~f:(fun builder (call, block, sub) ->
    GraphBuilder.add_call_edges builder call block ~caller:sub
  ) in
  builder.graph


module type ProblemSig = sig
  type t

  val merge: t -> t -> t
  val equal: t -> t -> Bool.t

  val update_def: t -> Def.t -> t
  val update_jump: t -> Jmp.t -> untaken_conditional_jump:(Jmp.t Option.t) -> t Option.t
  val update_call: t -> Call.t -> t
  val update_return: t -> value_before_call:(t Option.t) -> t
  val update_call_stub: t -> Call.t -> t Option.t
end

type 'a value =
  | Value of 'a
  | CallReturnCombinator of { call_value: 'a Option.t;
                              return_value: 'a Option.t; }

module InterproceduralFixpointProblem (FP : ProblemSig) : Fixpoint.ProblemSig with type t = FP.t value
                                                                               and type Node.t = Node.t
                                                                               and type EdgeLabel.t = EdgeLabel.t
= struct
  type t = FP.t value
    (*  | Value of FP.t
    | CallReturnCombinator of { call_value: FP.t Option.t;
      return_value: FP.t Option.t; } *)

  let unpack_value (value: t) : FP.t =
    match value with
    | Value inner -> inner
    | _ -> failwith "Error: Unexpected interprocedural fixpoint graph state"

  module Node = Node
  module EdgeLabel = EdgeLabel
  module BapGraph = Graph

  let merge (value1: t) (value2: t) : t =
    match (value1, value2) with
    | (Value val1, Value val2) -> Value (FP.merge val1 val2)
    | (CallReturnCombinator comb1, CallReturnCombinator comb2) ->
        CallReturnCombinator { call_value = Option.merge comb1.call_value comb2.call_value ~f:FP.merge;
                               return_value = Option.merge comb1.return_value comb2.return_value ~f:FP.merge }
    | _ -> failwith "Error: Unexpected interprocedural fixpoint graph state"

  let equal (value1: t) (value2: t) : Bool.t =
    match (value1, value2) with
    | (Value val1, Value val2) -> FP.equal val1 val2
    | (CallReturnCombinator comb1, CallReturnCombinator comb2) ->
        (Option.equal FP.equal comb1.call_value comb2.call_value) && (Option.equal FP.equal comb1.return_value comb2.return_value)
    | _ -> false

  let update_edge (value: t) (edge: BapGraph.Edge.t) : t Option.t =
    match Graph.Edge.label edge with
    | Block ->
        let block = match Graph.Edge.src edge with
          | BlkStart block -> block
          | _ -> failwith "Error: Malformed interprocedural fixpoint graph" in
        let defs = Term.enum def_t block in
        Some (Seq.fold defs ~init:value ~f:(fun value def_term ->
          Value (FP.update_def (unpack_value value) def_term) ) )
    | Jump (jump, untaken_conditional_jump) ->
        Option.find_map (FP.update_jump (unpack_value value) jump ~untaken_conditional_jump) ~f:(fun x -> Some (Value x))
    | Call call -> Some (Value (FP.update_call (unpack_value value) call))
    | ExternCallStub call ->
        Option.find_map (FP.update_call_stub (unpack_value value) call) ~f:(fun x -> Some (Value x))
    | CRCallStub _call ->
        Some (CallReturnCombinator { call_value = Some (unpack_value value);
                                     return_value = None })
    | CRReturnStub _block ->
        Some (CallReturnCombinator { call_value = None;
                                     return_value = Some (unpack_value value) })
    | CRCombine ->
        match value with
        | Value _ -> failwith "Error: Unexpected interprocedural fixpoint graph state"
        | CallReturnCombinator combinator ->
            begin match combinator.return_value with
            | Some ret -> Some (Value (FP.update_return ret ~value_before_call:combinator.call_value))
            | None -> None
            end
end



module InterproceduralFixpoint (FP: ProblemSig) : Fixpoint.FixpointSig with type node_label = Node.t
                                                                                               and type edge_label = EdgeLabel.t
                                                                                               and type value_type = FP.t value
  = Fixpoint.Fixpoint(InterproceduralFixpointProblem(FP))
