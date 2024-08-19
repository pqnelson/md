(**
Utility functions which are found in every other programming
language.
*)

(* list_indexof : ('a -> bool) -> 'a list -> int option

Returns the index of the first item in the list satisfying the
given predicate, if any.
 *)
local
  fun list_indexof_iter pred i (x::xs) =
      if pred x
      then SOME i
      else list_indexof_iter pred (i + 1) xs
    | list_indexof_iter pred i _ = NONE;
in
fun list_indexof (pred : 'a -> bool) (xs : 'a list) =
    list_indexof_iter pred 0 xs;
end;

(*
str_indexof : (char -> bool) -> string -> int option

Returns the index of the first character in the string
satisfying the predicate, if any.
*)
fun str_indexof (pred : char -> bool) (s : string) =
   (case CharVector.findi (fn (_,c) => pred c) s of
        NONE => NONE
      | SOME (i,c) => SOME i);

local
  fun string_indexof_iter (sub : string) (s : string) len (pos : int) =
    if "" = s orelse pos >= len
    then NONE
    else if (String.isPrefix sub s) orelse ("" = sub)
    then SOME pos
    else (case str_indexof (fn c => String.sub(sub,0) = c) s of
              SOME i' =>
              let
                val i = Int.max(i', 1)
              in
                string_indexof_iter sub
                                    (String.extract(s, i, NONE))
                                    len
                                    (pos + i)
              end
            | NONE => NONE);
in
fun string_indexof sub s =
    string_indexof_iter sub s (String.size s) 0
end;

(* Right is the result for the "correct" situation,
 Left is the result for the "erroneous" situation. *)
datatype ('a, 'b) Either = Left of 'a
                         | Right of 'b;

(* Print out a list of strings in a way that we can copy/paste
it back into the REPL *)
fun serialize_strings (strings : string list) =
    "["^(String.concatWith ", " strings)^"]";
