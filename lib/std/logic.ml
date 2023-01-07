open Base

module type Atom = sig
  type t

  include Comparator.S with type t := t

  val sexp_of_t : t -> Sexplib0.Sexp.t
  val fresh : unit -> t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val pp : t Fmt.t
  val show : t -> string
end

module type S = sig
  type atom
  type literal
  type clause
  type cnf
  type formula

  module Atom : Atom with type t = atom

  module Literal : sig
    type t = literal

    include Comparator.S with type t := t

    val atom : t -> atom
    val sexp_of_t : t -> Sexplib0.Sexp.t
    val not_ : t -> t
    val of_atom : atom -> t
    val is_positive : t -> bool
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val pp : t Fmt.t
    val show : t -> string
  end

  module Clause : sig
    type t
    include Comparator.S with type t := t

    val literals : t -> literal list
    val atoms : t -> atom list
    val of_atom : atom -> t
    val of_literal : literal -> t
    val of_literals : literal list -> t
    val or_ : t -> t -> t
    val or_list : t list -> t
    val contains : literal -> t -> bool
    val size : t -> int
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val pp : t Fmt.t
    val show : t -> string
  end

  module Cnf : sig
    type t = cnf

    val of_clause : clause -> t
    val of_clauses : clause list -> t
    val clauses : t -> clause list
    val literals : t -> literal list
    val atoms : t -> atom list
    val atom : atom -> t
    val top : t
    val bottom : t
    val and_ : t -> t -> t
    val and_list : t list -> t
    val add_clause : clause -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val pp : t Fmt.t
    val show : t -> string
    val is_pure : t -> bool
    val size : t -> int
  end

  module Formula : sig
    type t = formula

    val top : t
    val bottom : t
    val atom : atom -> t
    val and_ : t -> t -> t
    val or_ : t -> t -> t
    val imply : t -> t -> t
    val not_ : t -> t
    val and_list : t list -> t
    val or_list : t list -> t
    val fresh_atom : unit -> t
    val atoms : t -> atom list
    val literals : t -> literal list
    val subformulae : t -> t list
    val nnf : t -> t
    val cnf : t -> cnf
    val lit_is_pure : t -> literal -> bool
    val is_pure : t -> bool
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val depth : t -> int
    val size : t -> int
    val pp : t Fmt.t
    val show : t -> string
  end
end

module Make (A : Atom) = struct
  module Atom = A

  module Literal = struct
    module T = struct
      type t = Atom.t * bool [@@deriving compare, equal, sexp_of]
    end
    include T
    include Comparator.Make (T)

    let not_ (v, b) = (v, not b)
    let atom (v, _) = v
    let is_positive (_, b) = b
    let of_atom v = (v, true)

    let pp fmt = function
      | (v, true) -> Fmt.pf fmt "%a" Atom.pp v
      | (v, false) -> Fmt.pf fmt "¬%a" Atom.pp v

    let show = Util.show_of_pp pp
  end

  module Clause = struct
    module T = struct
      type t = (Literal.t, Literal.comparator_witness) Set.t

      let compare = Set.compare_m__t (module Literal)
      let sexp_of_t = Set.sexp_of_m__t (module Literal)
    end
    include T
    include Comparator.Make (T)

    let of_literal = Set.singleton (module Literal)
    let of_literals = Set.of_list (module Literal)
    let of_atom v = Literal.of_atom v |> of_literal
    let contains lit cla = Set.mem cla lit
    let or_ = Set.union
    let or_list = Set.union_list (module Literal)
    let literals = Set.to_list
    let atoms cla =
      List.fold_left (literals cla) ~init:(Set.empty (module Atom))
        ~f:(fun acc lit -> Set.add acc (Literal.atom lit))
      |> Set.to_list

    let compare cla1 cla2 = Set.compare_m__t (module Literal) cla1 cla2
    let equal cla1 cla2 = Set.equal_m__t (module Literal) cla1 cla2

    let pp fmt cla =
      let sep fmt () = Fmt.pf fmt "∨" in
      Fmt.list ~sep Literal.pp fmt (Set.to_list cla)

    let show cla = Util.show_of_pp pp cla

    let size = Set.length
  end

  module Cnf = struct
    type t = (Clause.t, Clause.comparator_witness) Set.t

    let of_clause = Set.singleton (module Clause)
    let of_clauses = Set.of_list (module Clause)

    let top = Set.empty (module Clause)
    let bottom =
      let a = Atom.fresh () |> Literal.of_atom in
      let b = Literal.not_ a in
      Clause.(or_ (of_literal a) (of_literal b)) |> of_clause

    let atom v = Clause.of_atom v |> of_clause
    let and_ = Base.Set.union
    let and_list = Base.Set.union_list (module Clause)

    let clauses = Set.to_list
    let add_clause cla p = Set.add p cla
    let literals p = Clause.or_list (clauses p) |> Clause.literals

    let atoms p =
      List.fold_left (literals p) ~init:(Set.empty (module Atom))
        ~f:(fun acc lit -> Set.add acc (Literal.atom lit))
      |> Set.to_list

    let lit_is_pure p lit =
      not @@ Set.exists p ~f:(Clause.contains lit)
      && not @@ Set.exists p ~f:(Clause.contains (Literal.not_ lit))

    let is_pure p = List.for_all (literals p) ~f:(lit_is_pure p)

    let compare p1 p2 = Set.compare_m__t (module Clause) p1 p2
    let equal p1 p2 = Set.equal_m__t (module Clause) p1 p2

    let pp fmt p =
      let sep fmt () = Fmt.pf fmt " ∧ " in
      let pp_clause fmt cla =
        if Clause.size cla > 1 then
          Fmt.pf fmt "(%a)" Clause.pp cla
        else Clause.pp fmt cla
      in
      Fmt.list ~sep pp_clause fmt (Set.to_list p)

    let show cla = Util.show_of_pp pp cla

    let size p =
      Set.fold p ~init:0 ~f:(fun acc cla -> acc + Clause.size cla)
      + Set.length p
  end

  module Formula = struct
    type form =
      | Atom of Atom.t
      | And_ of form * form
      | Or_ of form * form
      | Imply of form * form
      | Not_ of form
    [@@deriving compare, equal, sexp_of]

    type t =
      | Top
      | Bottom
      | Form of form
    [@@deriving compare, equal, sexp_of]

    let top = Top
    let bottom = Bottom

    let and_ p1 p2 =
      match p1, p2 with
      | Top, _ -> p2
      | _, Top -> p1
      | Bottom, _ | _, Bottom -> bottom
      | Form q1, Form q2 -> Form (And_ (q1, q2))

    let or_ p1 p2 =
      match p1, p2 with
      | Bottom, _ -> p2
      | _, Bottom -> p1
      | Top, _ | _, Top -> Top
      | Form q1, Form q2 -> Form (Or_ (q1, q2))

    let imply p1 p2 =
      match p1, p2 with
      | Bottom, _ -> Top
      | _, Bottom -> p1
      | Top, _ -> p2
      | _, Top -> Top
      | Form q1, Form q2 -> Form (Imply (q1, q2))

    let atom v = Form (Atom v)

    let not_ = function
      | Top -> Bottom
      | Bottom -> Top
      | Form (Not_ q) -> Form q
      | Form q -> Form (Not_ q)

    let fresh_atom () = Atom.fresh () |> atom

    let is_leaf = function
      | Atom _ | Not_ (Atom _) -> true
      | And_ _ | Or_ _ | Imply _ | Not_ _ -> false

    let rec pp_form fmt = function
      | Atom v -> Atom.pp fmt v
      | And_ (p1, p2) when is_leaf p1 && is_leaf p2 ->
          Fmt.pf fmt "%a ∧ %a" pp_form p1 pp_form p2
      | And_ (p1, p2) when is_leaf p2 ->
          Fmt.pf fmt "(%a) ∧ %a" pp_form p1 pp_form p2
      | And_ (p1, p2) when is_leaf p1 ->
          Fmt.pf fmt "%a ∧ (%a)" pp_form p1 pp_form p2
      | And_ (p1, p2) ->
          Fmt.pf fmt "(%a) ∧ (%a)" pp_form p1 pp_form p2
      | Or_ (p1, p2) when is_leaf p1 && is_leaf p2 ->
          Fmt.pf fmt "%a ∨ %a" pp_form p1 pp_form p2
      | Or_ (p1, p2) when is_leaf p2 ->
          Fmt.pf fmt "(%a) ∨ %a" pp_form p1 pp_form p2
      | Or_ (p1, p2) when is_leaf p1 ->
          Fmt.pf fmt "%a ∨ (%a)" pp_form p1 pp_form p2
      | Or_ (p1, p2) ->
          Fmt.pf fmt "(%a) ∨ (%a)" pp_form p1 pp_form p2
      | Imply (p1, p2) when is_leaf p1 && is_leaf p2 ->
          Fmt.pf fmt "%a ⇒ %a" pp_form p1 pp_form p2
      | Imply (p1, p2) when is_leaf p2 ->
          Fmt.pf fmt "(%a) ⇒ %a" pp_form p1 pp_form p2
      | Imply (p1, p2) when is_leaf p1 ->
          Fmt.pf fmt "%a ⇒ (%a)" pp_form p1 pp_form p2
      | Imply (p1, p2) ->
          Fmt.pf fmt "(%a) ⇒ (%a)" pp_form p1 pp_form p2
      | Not_ p when is_leaf p -> Fmt.pf fmt "¬%a" pp_form p
      | Not_ p -> Fmt.pf fmt "¬(%a)" pp_form p

    let pp fmt = function
      | Top -> Fmt.pf fmt "⊤"
      | Bottom -> Fmt.pf fmt "⊥"
      | Form q -> pp_form fmt q

    let nnf =
      let rec nnf_of_form = function
        |  Atom _ | Not_ (Atom _) as p -> p
        | And_ (p1, p2) ->
            let p1 = nnf_of_form p1 in
            let p2 = nnf_of_form p2 in
            And_ (p1, p2)
        | Or_ (p1, p2) ->
            let p1 = nnf_of_form p1 in
            let p2 = nnf_of_form p2 in
            Or_ (p1, p2)
        | Imply (p1, p2) -> nnf_of_form (Or_ ((Not_ p1), p2))
        | Not_ (Or_ (p1, p2)) -> nnf_of_form (And_ (Not_ p1, Not_ p2))
        | Not_ (And_ (p1, p2)) -> nnf_of_form (Or_ (Not_ p1, Not_ p2))
        | Not_ (Imply (p1, p2)) -> nnf_of_form (And_ (p1, (Not_ p2)))
        | Not_ (Not_ p1) -> nnf_of_form p1
      in function
      | Top | Bottom as p -> p
      | Form f -> Form (nnf_of_form f)

    let show = Util.show_of_pp pp

    let atoms =
      let rec atoms_of_form = function
        | Atom v -> [v]
        | Not_ p -> atoms_of_form p
        | And_ (p1, p2) -> atoms_of_form p1 @ atoms_of_form p2
        | Or_ (p1, p2) -> atoms_of_form p1 @ atoms_of_form p2
        | Imply (p1, p2) -> atoms_of_form p1 @ atoms_of_form p2
      in function
      | Top | Bottom -> []
      | Form f -> atoms_of_form f

    let literals =
      let rec literals_of_form is_pos = function
        | Atom v when is_pos -> [Literal.of_atom v]
        | Atom v -> [Literal.(not_ (of_atom v))]
        | Not_ p -> literals_of_form (not is_pos) p
        | And_ (p1, p2) ->
            literals_of_form is_pos p1 @ literals_of_form is_pos p2
        | Or_ (p1, p2) ->
            literals_of_form is_pos p1 @ literals_of_form is_pos p2
        | Imply (p1, p2) ->
            literals_of_form (not is_pos) p1 @ literals_of_form is_pos p2
      in function
      | Top | Bottom -> []
      | Form f -> literals_of_form true f

    let lit_is_pure p =
      let literals = literals p in
      fun lit ->
        not @@ List.mem literals lit ~equal:Literal.equal
        && not @@ List.mem literals (Literal.not_ lit) ~equal:Literal.equal

    let is_pure p = List.for_all (literals p) ~f:(lit_is_pure p)

    let depth =
      let rec depth_of_form = function
        | Atom _ -> 0
        | Not_ p -> 1 + depth_of_form p
        | And_ (p1, p2) | Or_ (p1, p2) | Imply (p1, p2) ->
            1 + Int.max (depth_of_form p1) (depth_of_form p2)
      in function
      | Top | Bottom -> 0
      | Form f -> depth_of_form f

    let size =
      let rec size_of_form = function
      | Atom _ -> 1
      | Not_ p -> 1 + size_of_form p
      | And_ (p1, p2) | Or_ (p1, p2) | Imply (p1, p2) ->
          1 + size_of_form p1 + size_of_form p2
      in function
      | Top | Bottom -> 1
      | Form f -> size_of_form f

    (* TODO: Optimize it *)
    let subformulae =
      let rec subformulae_of_form acc p =
        match p with
        | Atom _ -> Form p :: acc
        | Not_ q -> Form p :: (subformulae_of_form acc q)
        | And_ (q1, q2) | Or_ (q1, q2) | Imply (q1, q2) ->
            Form p :: ((subformulae_of_form acc q1)
            @ (subformulae_of_form acc q2))
      in function
      | Top | Bottom as p -> [p]
      | Form f -> subformulae_of_form [] f

    let and_list px = List.fold_left px ~init:Top ~f:and_

    let or_list px = List.fold_left px ~init:Bottom ~f:or_

    let cnf =
      let and_gate, or_gate, not_gate =
        (fun w z1 z2 ->
          Cnf.of_clauses Clause.[
            of_literals [Literal.not_ z1; Literal.not_ z2; w];
            of_literals [z1; Literal.not_ w];
            of_literals [z2; Literal.not_ w];
          ]),
        (fun w z1 z2 ->
          Cnf.of_clauses Clause.[
            of_literals [z1; z2; Literal.not_ w];
            of_literals [Literal.not_ z1; w];
            of_literals [Literal.not_ z2; w];
          ]),
        (fun w z ->
          Cnf.of_clauses Clause.[
            of_literals [Literal.not_ z; Literal.not_ w];
            of_literals [z; w]
          ])
      in
      let rec cnf_of_form acc w = function
        | Atom a -> Cnf.(add_clause (Clause.of_atom a)) acc
        | And_ (Atom a1, Atom a2) ->
            acc
            |> Cnf.(add_clause (Clause.of_atom a2))
            |> Cnf.(add_clause (Clause.of_atom a1))
        | And_ (Atom a, q) ->
            let z = Atom.fresh () |> Literal.of_atom in
            let gate = and_gate w (Literal.of_atom a) z in
            Cnf.and_ gate (cnf_of_form acc z q)
        | And_ (q, Atom a) ->
            let z = Atom.fresh () |> Literal.of_atom in
            let gate = and_gate w z (Literal.of_atom a) in
            Cnf.and_ gate (cnf_of_form acc z q)
        | And_ (q1, q2) ->
            let z1 = Atom.fresh () |> Literal.of_atom in
            let z2 = Atom.fresh () |> Literal.of_atom in
            let gate = and_gate w z1 z2 in
            Cnf.(and_ gate (and_ (cnf_of_form acc z1 q1) (cnf_of_form acc z2 q2)))
        | Or_ (Atom a1, Atom a2) ->
            Cnf.(add_clause (Clause.of_literals [Literal.of_atom a1; Literal.of_atom a2])) acc
        | Or_ (Atom a, q) ->
            let z = Atom.fresh () |> Literal.of_atom in
            let gate = or_gate w (Literal.of_atom a) z in
            Cnf.and_ gate (cnf_of_form acc z q)
        | Or_ (q, Atom a) ->
            let z = Atom.fresh () |> Literal.of_atom in
            let gate = or_gate w z (Literal.of_atom a) in
            Cnf.and_ gate (cnf_of_form acc z q)
        | Or_ (q1, q2) ->
            let z1 = Atom.fresh () |> Literal.of_atom in
            let z2 = Atom.fresh () |> Literal.of_atom in
            let gate = or_gate w z1 z2 in
            Cnf.(and_ gate (and_ (cnf_of_form acc z1 q1) (cnf_of_form acc z2 q2)))
        | Imply (p, q) -> cnf_of_form acc w (Or_ (Not_ p, q))
        | Not_ (Atom a) ->
            let gate = Literal.(not_ (of_atom a)) |> Clause.of_literal in
            Cnf.add_clause gate acc
        | Not_ p ->
            let z = Atom.fresh () |> Literal.of_atom in
            let gate = not_gate w z in
            Cnf.and_ gate (cnf_of_form acc z p)
      in function
      | Top -> Cnf.top
      | Bottom -> Cnf.bottom
      | Form f -> cnf_of_form Cnf.top (Atom.fresh () |> Literal.of_atom) f
  end

  type atom = Atom.t
  type clause = Clause.t
  type literal = Literal.t
  type cnf = Cnf.t
  type formula = Formula.t
end
