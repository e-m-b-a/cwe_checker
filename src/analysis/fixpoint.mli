(** This module implements a generic fixpoint algorithm for dataflow analysis.

    The usage works as follows:
    Write a module satisfying the signature defined in {!ProblemSig}.
    Then apply the {!Fixpoint} functor.
    The resulting module contains functions to create instances of the fixpoint problem
    and to compute the corresponding fixpoint. See {!FixpointSig} for more information.

    For general information on dataflow analysis using fixpoint algorithms see {{: https://en.wikipedia.org/wiki/Data-flow_analysis} Wikipedia}.
    Or open an issue on github that you want more documentation here. :-)
*)

(* open Bap.Std *)
open Core_kernel


(** Fixpoint problem module signature.

    It is assumed that the data type t forms a partially ordered set (poset) and that the merge
    function respects the poset structure, i.e. merge(t_1, t_2)>=t_1 and merge(t_1, t_2)>=t_2.
    The graph corresponding to the fixpoint problem only has transfer functions for edges (given by {!ProblemSig.update_edge}), not for nodes.
*)
module type ProblemSig = sig

  (** The data type of the fixpoint problem. *)
  type t

  (** The module containing the node type of the graph corresponding to the problem. *)
  module Node: Regular.Std.Opaque.S

  (** The module containing the type of the edge labels of the graph corresponding to the problem. *)
  module EdgeLabel: Core_kernel.T

  (** The module corresponding to the graph type of the problem.
      This solely depends on the node and edge label types and should be generated via
      {[
        Graphlib.Std.Graphlib.Make(Node)(EdgeLabel)
      ]}
  *)
  module BapGraph: Graphlib.Std.Graph with type node = Node.t and type Edge.label = EdgeLabel.t

  (** The merge function used to merge different values for a node into one value. *)
  val merge: t -> t -> t

  (** The comparison function used in the fixpoint algorithm. *)
  val equal: t -> t -> Bool.t

  (** The transition function for an edge.
      The function can return none to prevent any modification of the target node.
      This is useful when edges may be detected to be unreachable during analysis.
  *)
  val update_edge: t -> BapGraph.Edge.t -> t Option.t
end


(** The signature of a fixpoint module.

    The type t corresponds to a state in the fixpoint algorithm (initial, intermediate or finished).

    A new instance can be generated with the {!FixpointSig.empty} function.
    One can query the current values at each node via {!FixpointSig.get_node_value} and set the values via {!FixpointSig.set_node_value}.
    Note that the latter function adds the node to the worklist of nodes for which the algorithm has not stabilized yet.
    The current worklist can be queried with {!FixpointSig.get_worklist} and the function {!FixpointSig.compute} runs the fixpoint algorithm.
*)
module type FixpointSig = sig

  (** The type t of a Fixpoint module corresponds to a state in the fixpoint algorithm.
      This can be the initial state, the end state after the algorithm terminated, but also an intermediate state where the algorithm did not stabilize for all nodes yet.
  *)
  type t

  (** The type of the nodes of the corresponding graph. *)
  type node_label

  (** The type of the edge label of the corresponding graph. *)
  type edge_label

  (** The type of the values at each node. this is the type of the corresponding fixpoint problem. *)
  type value_type

  module BapGraph: Graphlib.Std.Graph with type node = node_label and type Edge.label = edge_label

  (** Generate an empty fixpoint solution for a graph.

      If there is no default value, the corresponding worklist will also be empty.
      If there is a default value, all nodes will be contained in the worklist, since there is a possibly unstabilized value at each node.
  *)
  val empty: ?default:value_type -> BapGraph.t -> t

  (** Get the value of the fixpoint algorithm at a node. *)
  val get_node_value: t -> node_label -> value_type Option.t

  (** Set the value at a node.

      This also adds the node to the worklist, since the new value may not be stable yet.
      If the fixpoint module has no default value for nodes, this is the only way to add initial values and to add nodes to the worklist. *)
  val set_node_value: t -> node_label -> value_type -> t

  (** Get a list of all nodes for which the algorithm did not stabilize yet. *)
  val get_worklist: t -> node_label List.t

  (** Run the fixpoint algorithm.

      If max_steps is not set, the algorithm runs until a fixpoint is reached.
      If max_steps is set, each node will be visited at most max_steps times before the algorithm terminates.
      Each node that was visited max_steps times, i.e. where no fixpoint was reached, is contained in the worklist after termination of the function.
  *)
  val compute: ?max_steps:Int.t -> t -> t
end


(** The fixpoint functor. Given a module satisfying the {!ProblemSig} signature it generates a fixpoint module that is used to compute the fixpoint. *)
module Fixpoint (FP : ProblemSig) : FixpointSig with type node_label := FP.Node.t
                                                      and type edge_label := FP.EdgeLabel.t
                                                      and type value_type := FP.t
                                                      and type BapGraph.t := FP.BapGraph.t
