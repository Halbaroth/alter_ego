open Base

module type Atom = sig
  (** {1 Types} *)

  type t
  (** Type of an atom. *)

  include Comparator.S with type t := t

  (** {1 Constructors and destructors} *)

  val sexp_of_t : t -> Sexplib0.Sexp.t
  (** [sexp_of_t at] produces a s-expression representation of the atom [at]. *)

  val fresh : unit -> t
  (** [fresh ()] produces a fresh atom. *)

  (** {1 Comparison} *)

  val compare : t -> t -> int
  (** [compare at1 at2] compares the atoms [at1] and [at2]. *)

  val equal : t -> t -> bool
  (** [equal at1 at2] tests the equality of the atom [at1] and [at2]. *)

  (** {1 Printing} *)

  val pp : t Fmt.t
  (** Pretty print an atom. *)

  val show : t -> string
  (** [show at] produces a representative string of the atom [at]. *)
end

module type S = sig
  type atom
  type literal
  type clause
  type cnf
  type formula

  module Atom : Atom with type t = atom

  module Literal : sig
    (** {1 Types} *)

    type t = literal
    (** Type of a literal. *)

    include Comparator.S with type t := t

    type set = (t, comparator_witness) Set.t
    (** Type of set of literals. *)

    (** {1 Constructors and destructors} *)

    val atom : t -> atom
    (** [atom lit] returns the atom associated with the literal [lit]. *)

    val sexp_of_t : t -> Sexplib0.Sexp.t
    (** [sexp_of_t lit] produces a s-expression representation of the literal
        [lit]. *)

    val not_ : t -> t
    (** [not_ lit] returns the negative form of the literal [lit]. *)

    val of_atom : atom -> t
    (** [of_atom at] creates the positive literal associated with the atom
        [at]. *)

    (** {1 Comparison} *)

    val sign : t -> bool
    (** [sign lit] is [true] if and only if the literal [lit] is positive. *)

    val compare : t -> t -> int
    (** [compare lit1 lit2] compares the literals [lit1] and [lit2]. *)

    val equal : t -> t -> bool
    (** [equal lit1 lit2] tests the equality of the literals [lit1] and
        [lit2]. *)

    (** {1 Printing} *)

    val pp : t Fmt.t
    (** Pretty print a literal. *)

    val show : t -> string
    (** [show lit] produces a representative string of the literal [lit]. *)
  end

  module Clause : sig
    (** {1 Types} *)

    type t
    (** Type of a clause. *)

    include Comparator.S with type t := t

    (** {1 Constructors and destructors} *)

    val literals : t -> literal list
    (** [literals cla] returns the list of literals appearing in [cla]. *)

    val atoms : t -> atom list
    (** [atoms cla] returns the list of atoms appearing in [cla]. *)

    val of_atom : atom -> t
    (** [of_atom at] creates the clause whose the only literal is the positive
        literal associated with the atom [at]. *)

    val of_literal : literal -> t
    (** [of_literal lit] produces the clause whose the only literal is [lit]. *)

    val of_literals : literal list -> t
    (** [of_literals lst] produces the clause whose the literals are the
        elements of the list [lst]. *)

    val or_ : t -> t -> t
    (** [or_ cla1 cla2] produces the disjunction of the clauses [cla1] and
        [cla2]. *)

    val or_list : t list -> t
    (** [or_list lst] produces the disjunctions of the clauses in the list
        [lst]. *)

    val contains : literal -> t -> bool
    (** [contains lit cla] is [true] if and only if the clause [cla] contains
        the literal [lit]. *)

    val size : t -> int
    (** [size cla] computes the size of the clause [cla],
        that is the number of literals. *)

    (** {1 Comparison and tests} *)

    val compare : t -> t -> int
    (** [compare cla1 cla2] compares the clauses [cla1] and [cla2]. *)

    val equal : t -> t -> bool
    (** [equal cla1 cla2] tests the equality of the clauses [cla1] and
        [cla2]. *)

    val is_sat : ass:Literal.set -> t -> bool
    (** [is_sat ~ass cla] returns [true] if the clause [cla] is satisfy by
        the assignement [ass]. *)

    val unit_lit : ass:Literal.set -> t -> literal option
    (** If [cla] is a unitary clause, [unit_lit ~ass cla] returns the only
        literal of [cla] who is not satisfied by the assignement [ass].
        Otherwise, the function returns [None]. *)

    (** {1 Printing} *)

    val pp : t Fmt.t
    (** Pretty print a clause. *)

    val show : t -> string
    (** [show cla] produces a representative string of the clause [cla]. *)
  end

  module Cnf : sig
    (** {1 Types} *)

    type t = cnf
    (** Type of a conjunctive normal form. *)

    (** {1 Conversion} *)

    val of_clause : clause -> t
    (** [of_clause cla] produces the cnf containing only the clause [cla]. *)

    val of_clauses : clause list -> t
    (** [of_clauses lst] produces the cnf whose the clauses are the elements of
        the list [lst]. *)

    val clauses : t -> clause list
    (** [clauses cnf] returns the list of clauses of cnf formula [cnf]. *)

    val literals : t -> literal list
    (** [literals cnf] returns the list of literals appearing in any clause
        of the cnf formula [cnf]. *)

    val atoms : t -> atom list
    (** [atoms cnf] returns the list of atoms appearing in any clause of the
        cnf formula [cnf]. *)

    (** {1 Smart constructors} *)

    val atom : atom -> t
    (** [atom at] constructs the cnf formula whose the only clause is the
        positive literal associated with the atom [at]. *)

    val top : t
    (** [top] is a cnf formula always true. *)

    val bottom : t
    (** [bottom] is a cnf formula always false. *)

    val and_ : t -> t -> t
    (** [and_ cnf1 cnf2] produces the conjunction of the cnf formulae [cnf1]
        and [cnf2]. *)

    val and_list : t list -> t
    (** [and_list lst] produces the conjunction of the cnf formulae in the list
        [lst]. *)

    val add_clause : clause -> t -> t
    (** [add_clause cla cnf] add the clause [cla] to the cnf formula [cnf]. *)

    val compare : t -> t -> int
    (** [compare cnf1 cnf2] compares the cnf formulae [cnf1] and [cnf2]. *)

    val equal : t -> t -> bool
    (** [equal cnf1 cnf2] tests the equality of the cnf formulae [cnf1] and
        [cnf2]. *)

    (** {1 Printing} *)

    val pp : t Fmt.t
    (** Pretty print a cnf. *)

    val show : t -> string
    (** [show cnf] produces a representative string of the cnf [cnf]. *)

    val is_pure : t -> bool
    (** [is_pure cnf] checks if all the literals of the cnf formula [cnf] are
        pure in [cnf]. *)

    val size : t -> int
    (** [size cnf] returns the size of cnf formula [cnf], that is the number
        of clauses of [cnf]. *)
  end

  module Formula : sig
    (** {1 Types} *)

    type t = formula
    (** Type of a formula. *)

    (** {1 Smart constructors} *)

    val top : t
    (** [top] is a formula always true. *)

    val bottom : t
    (** [bottom] is a formula always false. *)

    val atom : atom -> t
    (** [atom at] produces the atomic formula associated with the atom [at]. *)

    val and_ : t -> t -> t
    (** [and_ form1 form2] produces an equivalent formula to the conjunction
        of the formulae [form1] and [form2]. *)

    val or_ : t -> t -> t
    (** [or_ form1 form2] produces an equivalent formula to the disjunction
        of the formulae [form1] and [form2]. *)

    val imply : t -> t -> t
    (** [imply_ form1 form2] produces an equivalent formula to the implication
        of the formula [form2] by the formula [form1]. *)

    val not_ : t -> t
    (** [not_ form] produces an equivalent formula to the negation of
        the formula [form]. *)

    val and_list : t list -> t
    (** [and_list list] produces an equivalent formula to the conjunction
        of the formulae in the list [lst]. *)

    val or_list : t list -> t
    (** [or_list lst] produces an equivalent formula to the disjunction
        of the formulae in the list [lst]. *)

    val fresh_atom : unit -> t
    (** [fresh_atom ()] produces a fresh atomic formula. *)

    (** {1 Destructors} *)

    val atoms : t -> atom list
    (** [atoms form] returns the list of the atoms appearing in the formula
        [form]. *)

    val literals : t -> literal list
    (** [literal form] returns the list of the literals appearing in the
        formula [form]. *)

    val subformulae : t -> t list
    (** [subformulae form] returns a list of the subformulae of [form].
        The form itself is included. *)

    (** {1 Transformations} *)

    val nnf : t -> t
    (** [nnf form] returns a negative normal form of the formula [form]. *)

    val cnf : t -> cnf
    (** [cnf form] returns a conjunctive normal form of the formula [form]. *)

    (** {1 Comparison and tests} *)

    val lit_is_pure : t -> literal -> bool
    (** [lit_is_pure form lit] is [true] if and only if the literal [lit] is
        pure in the formula [form]. A literal is pure in a formula if all its
        occurrances are positive, respectively negative, in this formula. *)

    val is_pure : t -> bool
    (** [is_pure form] is [true] if and only if [form] contains only pure
        literals. @see {!lit_is_pure} for the definition of pureness in this
        context. *)

    val compare : t -> t -> int
    (** [compare form1 form2] compares the formulae [form1] and [form2]. *)

    val equal : t -> t -> bool
    (** [equal form1 form2] tests the equality of the formulae [form1] and
        [form2]. *)

    (** {1 Misc} *)

    val depth : t -> int
    (** [depth form] returns the [depth] of the formula as a tree. *)

    val size : t -> int
    (** [size form] returns the size of formula [form]. *)

    (** {1 Printing} *)

    val pp : t Fmt.t
    (** Pretty print a formula. *)

    val show : t -> string
    (** [show form] produces a representative string of the formula [form]. *)
  end
end

module Make (A : Atom) : S with type atom = A.t
