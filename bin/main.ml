open Base
open Alter_ego.Lib

module Atom = struct
  module T = struct
    type t = string

    let compare = String.compare
    let equal = String.equal
    let sexp_of_t = Sexplib.Std.sexp_of_string
  end

  include T
  include Comparator.Make (T)

  let fresh =
    let counter = ref 0 in
    fun () ->
      let str = Fmt.str "X_%i" !counter in
      Int.incr counter;
      str

  let pp fmt = Fmt.pf fmt "%s"
  let show = Std.Util.show_of_pp pp
end

module Logic = Std.Logic.Make (Atom)

let () =
  let open Logic in
  Fmt.pr "@.";
  let x = Atom.fresh () |> Formula.atom in
  let y = Atom.fresh () |> Formula.atom in
  let f = Formula.(and_ x y) in
  Fmt.pr "%a@." Formula.pp f;
  let g = Formula.cnf f in
  Fmt.pr "%a@." Cnf.pp g
