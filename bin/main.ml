open Base
open Alter_ego.Lib


let () =
  let open Logic.Prop in
  Fmt.pr "@.";
  let x = Atom.fresh () |> Formula.atom in
  let y = Atom.fresh () |> Formula.atom in
  let f = Formula.(and_ x y) in
  Fmt.pr "%a@." Formula.pp f;
  let g = Formula.cnf f in
  Fmt.pr "%a@." Cnf.pp g
