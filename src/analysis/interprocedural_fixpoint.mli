(** This module implements a functor for the generation of fixpoint problems for interprocedural dataflow analysis.

    Basic usage: Implement a module satisfying the signature defined in {!ProblemSig}.
    Then apply the {!InterproceduralFixpoint} functor.
    The control flow graph of a program necessary for the fixpoint algorithm can be generated with the function {!generate_fixpoint_cfg}.

    Function calls are handled by the fixpoint algorithm as follows:
    - For calls to library functions outside of the binary a function {!ProblemSig.update_call_stub} must be provided by the user.
    This way the user can implement call-specific stubs for the algorithm tu use.
    - For calls to functions inside the binary a function {!ProblemSig.update_return} must be provided by the user.
    This functions gets the state of the fixpoint algorithm at the call-site as input.
    This way the user can save caller-specific data at the call-site and recover it later at the return-site.
    Internally, this is implemented as follows:
    For each call a dummy node called CallReturn is added to the control flow graph together with edges from the call-site and from the return-site to this node.
    The node just gathers information from the call- and return blocks
    that get combined (using the {!ProblemSig.update_return} function) on an edge from CallReturn to the actual return block.

    TODO:
    - Indirect function calls are ignored right now. A mechanism to specify targets for indirect function calls needs to be implemented.
    - Interrupts are not handled at the moment, i.e. there are no edges added to the control flow graph for interrupts.
*)

open Bap.Std
open Core_kernel


(** The Node module contains the type of a node in the interprocedural fixpoint graph.

    There are three types of nodes:
    - BlkStart nodes for the start of a basic block
    - BlkEnd nodes for the end of a basic block
    - CallReturn nodes are inserted between basic blocks ending in a call instruction and the block that the call returns to.
    See the module level documentation for their use.
*)
module Node : sig
  type t =
    | BlkStart of Blk.t
    | BlkEnd of Blk.t
    | CallReturn of Blk.t (* The Blk.t is the block from the call instruction *)
  [@@deriving bin_io, compare, sexp, hash]

  include Regular.Std.Opaque.S with type t := t
end


(** The EdgeLabel module contains the type of the edge labels in the interprocedural fixpoint graph.

    The type of edges are:
    - Block: An edge from the BlkStart to the BlkEnd node of a basic block.
    - Jump: An edge corresponding to a jump instruction. If the jump was preceded by a conditional jump (which was not taken if this jump is taken),
    then the untaken jump instruction is passed in the Jmp.t Option.t.
    - Call: An edge from a block ending in a call instruction to the starting block of the called function.
    - CRCallStub, CRReturnStub, CRCombine: See the module-level documentation.
*)
module EdgeLabel : sig
  type t =
    | Block
    | Jump of Jmp.t * (Jmp.t Option.t) (* The second value is an untaken conditional jump instruction *)
    | Call of Call.t
    | ExternCallStub of Call.t
    | CRCallStub of Call.t
    | CRReturnStub of Blk.t
    | CRCombine
end


(** The value data type that is saved at each node during the fixpoint algorithm.

    This is either Value('a) for BlkStart and BlkEnd nodes or CallReturnCombinator for the artificial CallReturn nodes.
    The CallReturnCombinator just saves the values before the call and on return so that they can be combined
    using the update_return function on the outgoing edge.
*)
type 'a value =
  | Value of 'a
  | CallReturnCombinator of { call_value: 'a Option.t;
                              return_value: 'a Option.t; }


(** The graph type of the control flow graph for interprocedural fixpoint problems. *)
module Graph : Graphlib.Std.Graph with type node = Node.t
                                   and type Node.label = Node.t
                                   and type Edge.label = EdgeLabel.t


(** This function generates the control flow graph used for the interprocedural fixpoint algorithm for a given project. *)
val generate_fixpoint_cfg: Project.t -> Graph.t


(** Each interprocedural fixpoint problem has to satisfy the interface defined in the ProblemSig module signature. *)
module type ProblemSig = sig

  (** The data type of the fixpoint problem *)
  type t

  (** The merge function.

      It is assumed that the type t forms a partially ordered set with respect to the merge and equal functions,
      i.e. merge(x_1, x_2) >= x_1 and merge(x_1, x_2) >= x_2 holds for all x_1, x_2. *)
  val merge: t -> t -> t

  (** The equality function for the data type t.

      It is assumed that the usual axioms for equality functions hold for this function. *)
  val equal: t -> t -> Bool.t

  (** The update_def function is called for each definition in a basic block in order
      to form the transfer function from the start to the end of a basic block. *)
  val update_def: t -> Def.t -> t

  (** The update_jump function is the transfer function for conventional jumps.
      If the optional argument is set, then the jump is only taken if the conditional jump in the optional argument was not taken. *)
  val update_jump: t -> Jmp.t -> untaken_conditional_jump:(Jmp.t Option.t) -> t Option.t

  (** The transfer function from a call instruction to the start block of the called function if the callee is not an extern function call.*)
  val update_call: t -> Call.t -> t

  (** The update_return function is the transfer function from the last block of a called function to the return site in the calling function.
      To recover caller-specific data, the state of the fixpoint algorithm at the callsite is also provided as an optional argument.
      See also the module-level documentation. *)
  val update_return: t -> value_before_call:(t Option.t) -> t

  (** The update_call_stub function is the transfer function for calls to functions outside of the binary.
      The corresponding edge in the control flow graph goes directly from the call-site to the block that the call returns to.
      For calls to non-returning functions this function should return None. *)
  val update_call_stub: t -> Call.t -> t Option.t
end


(** This functor generates a fixpoint module from an interprocedural fixpoint problem satisfying the ProblemSig interface.

    The corresponding fixpoint can be computed with the compute function of the returned fixpoint module.
*)
module Make (FP: ProblemSig) : Fixpoint.FixpointSig with type Graph.node := Node.t
                                                                        and type Graph.Edge.label := EdgeLabel.t
                                                                        and type value_type := FP.t value
                                                                        and type Graph.t := Graph.t
