module Std = Alter_ego__std
open Base

include Basic.Make (struct
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
end)
