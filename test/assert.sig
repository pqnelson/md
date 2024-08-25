signature ASSERT = sig
  exception Failure of string;

  val !! : string -> bool -> unit;
  val eq : ''a * ''a * string -> unit;
end;
