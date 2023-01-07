let show_of_pp : 'a Fmt.t -> 'a -> string = fun pp x ->
  pp Caml.Format.str_formatter x;
  Caml.Format.flush_str_formatter ()

