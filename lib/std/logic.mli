open Base

module type Atom = sig
  type t
  (** Type of an atom. *)

  include Comparator.S with type t := t

  val fresh : unit -> t
  (** [fresh ()] produces a fresh atom. *)

  val compare : t -> t -> int
  (** [compare a1 a2] compares the atoms [a1] and [a2]. *)

  val equal : t -> t -> bool
  (** [equal a1 a2] tests the equality of the atom [a1] and [a2]. *)

  val sexp_of_t : t -> Sexplib0.Sexp.t
  val pp : t Fmt.t
  (** Pretty print an atom. *)

  val show : t -> string
  (** [show a] produces a representative string of the atom [a]. *)
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

    val not_ : t -> t
    val of_atom : atom -> t
    val atom : t -> atom
    val is_positive : t -> bool
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val sexp_of_t : t -> Sexplib0.Sexp.t
    val pp : t Fmt.t
    val show : t -> string
  end

  module Clause : sig
    type t

    include Comparator.S with type t := t

    val of_literal : literal -> t
    val of_literals : literal list -> t
    val of_atom : atom -> t
    val contains : literal -> t -> bool
    val or_ : t -> t -> t
    val or_list : t list -> t
    val literals : t -> literal list
    val atoms : t -> atom list
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
    val atom : atom -> t
    val top : t
    val bottom : t
    val and_ : t -> t -> t
    val and_list : t list -> t
    val add_clause : clause -> t -> t
    val clauses : t -> clause list
    val literals : t -> literal list
    val atoms : t -> atom list
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
    val lit_is_pure : t -> literal -> bool
    val is_pure : t -> bool

    val depth : t -> int
    val size : t -> int
    val subformulae : t -> t list

    val nnf : t -> t
    val cnf : t -> cnf

    val compare : t -> t -> int
    val equal : t -> t -> bool
    val pp : t Fmt.t
    val show : t -> string
  end
end

module Make (A : Atom) : S with type atom = A.t
