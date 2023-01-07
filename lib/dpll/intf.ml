module type S = sig
  (** {1 Types} *)

  type t
  (** Type of the context of the DPLL solver. *)

  module Logic : Alter_ego__std.Logic.S

  type status = Unsat | Sat

  val initial : t
  (** [inital] is the inital context. *)

  val add_clause : Logic.clause -> t -> t
  val add_form : Logic.cnf -> t -> t
  val sat : t -> status
end
