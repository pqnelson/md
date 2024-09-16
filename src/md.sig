signature MD = sig
  (* Metadata, the "YAML header", represented as key-value
     pairs. *)
  type metadata = (string * string) list;
  val parse : string -> metadata * ((string Block) list);
  val parse_block : substring -> string Block list;
  val parse_inline : string -> Inline list;
end;
