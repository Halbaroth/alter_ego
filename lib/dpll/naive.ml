open Base

module Make (Logic : Alter_ego__std.Logic.S) = struct
  open Logic

  type t = Literal.set
  type status = Unsat | Sat

  module Logic = Logic

  let initial = Set.empty (module Literal)
  let add_clause _ _ = initial
  let add_form _ _ = initial
  let sat _ = Unsat
  let bcp t = t
end
