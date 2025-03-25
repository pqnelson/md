signature EMITTER = sig
  (* emit (string -> string) -> string Block list -> string

  The first argument is a syntax highlighting function, which
  should also perform any HTML escapes (or analogous
  preprocessing) in addition to formatting.
   *)
  val emit : (string -> string -> string) -> string Block list -> string;
end;
