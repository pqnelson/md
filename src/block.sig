
(*
In a list, each <li> may consist of multiple blocks.
And a <ol> (resp., <ul>) is just a list of <li> elements.
*)
datatype 'a Block =
         Par of Inline list
       | Pre of 'a * (string option)
       | Heading of int * (Inline list)
       | Quote of ('a Block list)
       | UList of (('a Block list) list)
       | OList of (('a Block list) list);

signature BLOCK = sig
  val map : ('a Block -> 'b Block) -> 'a Block -> 'b Block;
  val app : ('a Block -> unit) -> 'a Block -> unit;
  val serialize : ('a -> string) -> 'a Block -> string;
end;

