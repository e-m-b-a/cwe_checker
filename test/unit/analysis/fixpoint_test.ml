(** This module contains test cases for the implementation of the fixpoint algorithms. *)

open Bap.Std
open Core_kernel
open Cwe_checker_core


let example_project = ref None


module TestGraph = Graphlib.Std.Graphlib.Make(Int)(Int)

let check msg x = Alcotest.(check bool) msg true x

module TestProblem = struct
  type t = Int.t

  module Node = Int
  module EdgeLabel = Int
  module BapGraph = TestGraph

  let merge (val1: Int.t) (val2: Int.t) : Int.t =
    Int.min val1 val2

  let equal = Int.equal

  let update_edge (value: t) (edge: BapGraph.Edge.t) : t Option.t =
    Some (value + BapGraph.Edge.label edge)
end


let test_shortest_path () =
  let module FP = Fixpoint.Fixpoint(TestProblem) in
  let module G = FP.BapGraph in
  let graph = ref G.empty in
  let () = for i = 1 to 10 do
      graph := G.Edge.insert (G.Edge.create i (i + 1) i) !graph
    done in
  graph := G.Edge.insert (G.Edge.create 10 1 1) !graph;
  let graph = !graph in
  let problem = FP.empty graph in
  check "shortest_path_empty_initial_worklist" (FP.get_worklist problem = []);
  let problem = FP.set_node_value problem 7 0 in
  check "shortest_path_initial_worklist" (FP.get_worklist problem = 7 :: []);
  let solution = FP.compute problem in
  check "shortest_path_fixpoint1" ((FP.get_node_value solution 1) = (Some 25));
  let graph = G.Edge.insert (G.Edge.create 7 1 20) graph in
  let problem = FP.empty graph in
  let problem = FP.set_node_value problem 7 0 in
  let solution = FP.compute ~max_steps:10 problem in
  check "shortest_path_fixpoint2" ((FP.get_node_value solution 1) = (Some 20));
  check "shortest_path_no_worklist" (FP.get_worklist solution = []);
  let graph = G.Edge.insert (G.Edge.create 7 6 (1 - 9)) graph in
  let problem = FP.empty graph in
  let problem = FP.set_node_value problem 7 0 in
  let solution = FP.compute ~max_steps:10 problem in
     check "shortest_path_with_worklist" (FP.get_worklist solution <> [])

let ignore _x = ()

module CallDepthTest = struct
  type t = {
    cur_depth : Int.t;
    max_depth : Int.t
  }

  let merge (val1: t) (val2: t) : t =
    { cur_depth = Int.max val1.cur_depth val2.cur_depth;
      max_depth = Int.max val1.max_depth val2.max_depth
    }

  let equal (val1: t) (val2: t) : Bool.t =
    (Int.equal val1.cur_depth val2.cur_depth) && (Int.equal val1.max_depth val2.max_depth)

  let update_def (value: t) (_def: Def.t) : t =
    value

  let update_jump (value: t) (_jump: Jmp.t) ~(untaken_conditional_jump:(Jmp.t Option.t)) : t Option.t =
    let () = ignore untaken_conditional_jump in
    Some value

  let update_call (value: t) (_call: Call.t) : t =
    { cur_depth = value.cur_depth + 1;
      max_depth = Int.max value.max_depth (value.cur_depth + 1) }

  let update_return (value: t) ~(value_before_call:t Option.t) : t =
    match value_before_call with
    | Some caller_value ->
        { cur_depth = caller_value.cur_depth;
          max_depth = value.max_depth }
    | None ->
        { cur_depth = 0;
          max_depth = 0 }

  let update_call_stub (value: t) (_call: Call.t) : t Option.t =
    Some value
end

let test_call_depth () =
  let project = Option.value_exn !example_project in
  let module FP = Interprocedural_fixpoint.InterproceduralFixpoint(CallDepthTest) in
  let graph = Interprocedural_fixpoint.generate_fixpoint_cfg project in
  let problem = FP.empty graph in
  let entry_fns = Symbol_utils.get_program_entry_points (Project.program project) in
  let problem = Seq.fold entry_fns ~init:problem ~f:(fun problem entry_fn ->
    match (Term.first blk_t entry_fn) with
    | Some entry_block ->
        FP.set_node_value problem (BlkStart entry_block) (Value { CallDepthTest.cur_depth = 1; max_depth = 1 })
    | None -> problem
  ) in
  let solution = FP.compute ~max_steps:10 problem in
  let all_fns = Term.enum sub_t (Project.program project) in
  let max_depth = Seq.fold all_fns ~init:0 ~f:(fun max_depth func ->
    let all_blocks = Term.enum blk_t func in
    Seq.fold all_blocks ~init:max_depth ~f:(fun max_depth block ->
      match FP.get_node_value solution (BlkStart block) with
      | Some (Value elem) ->
          Int.max max_depth elem.max_depth
      | _ -> max_depth
    )
  ) in
  check "call_depth" (max_depth = 2)

let tests = [
  "Shortest Path", `Quick, test_shortest_path;
  "Call Depth", `Quick, test_call_depth;
]
