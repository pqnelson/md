signature MD = sig
  val parse : string -> (string Block) list;
  val parse_block : string list -> string Block list;
  val parse_inline : string -> Inline list;
end;
