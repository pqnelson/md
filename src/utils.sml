(**
Utility functions which are found in every other programming
language.

The convention is to name them as `<module>_<function name>`,
all lowercased with `snake_case`.
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

(* string_replace_all : string -> string -> string

Given a `sub` string which possibly appears in `s`, replace all
instances of them in `s` by a `new` string.

If `sub` is not a substring of `s`, then `s` is returned
unchanged.
*)
local
  fun str_replace_iter sub s new len acc =
      case string_indexof sub s of
          NONE => acc ^ s
                            (* s = pre ^ sub ^ post *)
        | SOME i =>
          let
            val pre = String.substring(s,0,i);
            val post = String.extract(s,i+len,NONE);
          in 
            str_replace_iter sub post new len (acc^pre^new)
          end;
in
fun string_replace_all sub s new =
    if not String.isSubstring sub s
    then s
    else str_replace_iter sub s new (String.size sub) ""
end;

(* Right is the result for the "correct" situation,
 Left is the result for the "erroneous" situation. *)
datatype ('a, 'b) Either = Left of 'a
                         | Right of 'b;

(* Print out a list of strings in a way that we can copy/paste
it back into the REPL *)
fun serialize_strings (strings : string list) =
    "["^(String.concatWith ", " strings)^"]";

(* id : 'a -> 'a
The fabled identity function. *)
fun id x = x;


val string_to_lower =
    String.translate (String.str o Char.toLower);

val string_to_upper =
    String.translate (String.str o Char.toUpper);

(* trim_s : string -> string

Removes all leading and trailing whitespace.

Examples:
  string_trim "  foo" = "foo";
  string_trim "bar and eggs   " = "bar and eggs";
  string_trim "    spam eggs" = "spam eggs";
  string_trim "unchanged string" = "unchanged string";
*)
fun string_trim str =
    let
      fun pre_trim_iter "" = ""
        | pre_trim_iter s = 
          if Char.isSpace (String.sub(s,0))
          then pre_trim_iter (String.extract(s,1,NONE))
          else s;
      fun post_trim_iter "" = ""
        | post_trim_iter s =
          let
            val len = String.size s;
          in
            if Char.isSpace (String.sub(s,len-1))
            then post_trim_iter(String.substring(s,0,len-1))
            else s
          end;
    in
      post_trim_iter (pre_trim_iter str)
    end;

fun is_md "" = false
  | is_md file_name = String.isSuffix ".md" (string_to_lower file_name);

fun md_to_html s =
    if is_md s
    then (String.substring(s, 0, String.size(s) - 2) ^ "html")
    else s;
