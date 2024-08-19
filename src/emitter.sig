signature EMITTER = sig
  (* ('a -> string) preprocesses the code blocks *)
  val emit : ('a -> string) -> 'a Block list -> string;
end;
