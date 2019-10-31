(** This module contains test cases for the implementation of the fixpoint algorithms. *)

open Bap.Std
open Core_kernel
open Cwe_checker_core


let check msg x = Alcotest.(check bool) msg true x

module TestProblem = struct
  type t = Int.t

  module Node = Int
  module EdgeLabel = Int
  module BapGraph = Graphlib.Std.Graphlib.Make(Node)(EdgeLabel)

  let merge (val1: Int.t) (val2: Int.t) : Int.t =
    Int.min val1 val2

  let equal = Int.equal

  let update_edge (value: t) (edge: BapGraph.Edge.t) : t Option.t =
    Some (value + 1)
end


let test_shortest_path () = ()


let tests = [
  "Shortest Path", `Quick, test_shortest_path;
]
