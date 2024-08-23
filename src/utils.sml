(**
Utility functions which are found in every other programming
language.

The convention is to name them as `<module>_<function name>`,
all lowercased with `snake_case`.
*)

(* id : 'a -> 'a
The fabled identity function. *)
fun id x = x;

(* Right is the result for the "correct" situation,
 Left is the result for the "erroneous" situation. *)
datatype ('a, 'b) Either = Left of 'a
                         | Right of 'b;

(** # List Utility functions *)

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

(** # String utility functions *)

(*
str_indexof_from : (char -> bool) -> string -> int option

Returns the index of the first character in the string
satisfying the predicate, if any.
*)
fun str_indexof (pred : char -> bool) (s : string) =
   (case CharVector.findi (fn (_,c) => pred c) s of
        NONE => NONE
      | SOME (i,c) => SOME i);

fun str_indexof_from (pred : char -> bool) (s : string) start =
    if (start >= String.size s)
    then NONE
    else if pred (String.sub(s, start))
    then SOME start
    else str_indexof_from pred s (start + 1);

(*
string_indexof_from : string -> string -> int -> int option

Returns the index of the first character in the string after
`start` which starts a substring matching the given `sub`
string, if any exists.
*)
local
  fun matches_at sub s len pos =
      ((String.size(sub)) + pos < len) andalso
      (EQUAL =
       Substring.compare(Substring.extract(sub, 0, NONE),
                         Substring.substring(s, pos, String.size(sub))));
  fun string_indexof_iter (sub : string) (s : string) len (pos : int) =
    if pos >= len
    then NONE
    else (case str_indexof_from (fn c => String.sub(sub,0) = c) s pos of
            SOME i =>
            if matches_at sub s len i
            then SOME i
            else string_indexof_iter sub s len (i + 1)
           | NONE => NONE);
in
fun string_indexof_from sub s start =
    if "" = s orelse start >= String.size s
    then NONE
    else if Substring.isPrefix sub (Substring.extract(s,start,NONE))
    then SOME start
    else string_indexof_iter sub s (String.size s) start
end;

(*
string_indexof : string -> string -> int option

Returns the index of the first character in the string
satisfying the predicate, if any.
*)
fun string_indexof sub s =
    string_indexof_from sub s 0;

(* string_replace_all : string -> string -> string

Given a `sub` string which possibly appears in `s`, replace all
instances of them in `s` by a `new` string.

If `sub` is not a substring of `s`, then `s` is returned
unchanged.
*)
local
  fun str_replace_iter sub s new len acc =
      case string_indexof sub s of
          NONE => if "" = acc then s else (acc ^ s)
        | SOME i => (* s = pre ^ sub ^ post *)
          let
            val pre = String.substring(s,0,i);
            val post = String.extract(s,i+len,NONE);
          in 
            str_replace_iter sub post new len (acc ^ pre ^ new)
          end;
in
fun string_replace_all sub s new =
    str_replace_iter sub s new (String.size sub) ""
end;

(* serialize_strings : string list -> string.

Print out a list of strings in a way that we can copy/paste
it back into the REPL *)
fun serialize_strings (strings : string list) =
    "["^(String.concatWith ", " strings)^"]";

(* string_to_lower : string -> string

Converts all of the characters in this string to lower case,
always returns a new string.
*)
val string_to_lower : string -> string =
    CharVector.map Char.toLower;

(* string_to_upper : string -> string

Converts all of the characters in this string to upper case,
always returns a new string.
 *)
val string_to_upper : string -> string =
    CharVector.map Char.toUpper;

(* string_trim : string -> string

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

(* is_md : string -> bool

Test if a string is the name for a Markdown file. *)
fun is_md "" = false
  | is_md file_name = String.isSuffix ".md" (string_to_lower file_name);

(* md_to_html : string -> string

Transform a Markdown file name into an HTML file name.

If the given string does not look like a Markdown file name,
then we just return it back to the user. This makes it
idempotent. 
*)
fun md_to_html s =
    if is_md s
    then (String.substring(s, 0, String.size(s) - 2) ^ "html")
    else s;
