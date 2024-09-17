(** Parser combinator for a sensible subset of Markdown.

# Blockquotes

Blockquotes begin a line with `> `, but nested blockquotes may
begin with `>>`, `>>>`, etc. Lists within a blockquote may also
omit the space `>- First item`, etc.

# Code blocks

Codefences start a newline with the triple backtick ("`` ``` ``")
followed by the language name and whatever other metadata (for
example, `` ```sml {file=whatever.sml} ``).

Code fences end with `` ``` `` on their own line.

If you want to give an example of some block of markdown, you
must add some prefatory spaces. For example:

```md
  This is some example
  paragraph which rambles on.

  ```sml
  fun main _ = (* do stuff *)
  val _ = main ();
  ```

  Now we talk about the code. Blah blah blah blah.
```

Right now, code blocks are treated as just text without syntax
highlighting. But `Block` is abstracting out the _type_ of a
code block, so if you wanted to run it through a syntax
highlighter, you can do that.

# Lists

Unordered lists must be indented, may use `+` or `-` for each
item (consistently --- all of them must be `+` or all of them
must be `-`).
```
- Leading line
  continues here
  and continues here
  - But here is a nested unordered list
  - With a second nested item
    whose line spills over here
- Second item on the outermost unordered list
```

Ordered lists have items begin with `[digit]+. ` The whitespace
is critical. Again, indentation must be the number of digits
plus two. For example:
```
99. This is the first item,
    even though it is numbered "99".
100. This is the second item,
     and the indentation must be 5 spaces.
```

# Headers

Headings are of the form:
```
# Heading 1
## Heading 2
### Heading 3
#### Heading 4
```
Each heading starts on a newline, with 1 or more hashtags
(depending on the level of the heading).

If there are 6 or more hashtags, then it is rounded down to be
the 6th level.

# Inline markup

We use `**bold**`, `` `code` `` (double backtick and a space if
you want backticks captured as part of the code), `_italics_`,
`[link name](url)`, `[#anchor]` (to produce a `<a id="#anchor" />`
element for HTML), and `![image description](url)`.

@author Alex Nelson <pqnelson@gmail.com>

@license MIT License

@version 0.0.1
 *)

structure Md : MD = struct

type metadata = (string * string) list;

(* parse_inline : string -> Inline list

Given a "paragraph" of text (i.e., a bunch of lines of
nonwhitespace text, separated by newlines), parse it to produce
a list of "inline" elements.

Will skip over anything within $...$, \(...\), $$...$$, \[...\],
\begin{equation}...\end{equation},
\begin{equation*}...\end{equation*},
or other LaTeX macrkup, and treat these as "just" text.
 *)

(* TODO:

Inline code, for Github pages, uses `code snippet`{:.language}
for highlighting inline code. This seems fine, I should try to
adhere to this spec.

Pandoc uses `code snippet`{.language} --- perhaps I should try
to support both?

@see https://kramdown.gettalong.org/syntax.html#span-ials
@see https://pandoc.org/MANUAL.html#verbatim
*)

fun inline_code s len start =
  let
    val lexeme = String.extract(s, start, NONE);
    val prefix = if 0 = start
                 then NONE
                 else SOME(String.extract(s, 0, SOME start));
  in
    case str_indexof (fn c => #"`" <> c) lexeme of
        NONE => Left 1
      | SOME i =>
        let
          val token = implode(List.tabulate (i, fn _ => #"`"));
          val token_len = String.size token;
          val post_token_lexeme = (String.extract(lexeme, i, NONE));
          (* post_token_lexeme looks like "foo ... bar` rest"
           or "foo ... bar `` rest". *)
          val post_token_len = String.size(post_token_lexeme);
          fun is_ending_delim idx =
              (idx = 0 orelse
               #"`" <> String.sub(post_token_lexeme, idx - 1)) andalso
              (post_token_len = (idx + token_len) orelse
               (#"`" <> String.sub(post_token_lexeme,
                                   idx + token_len)));
          fun try_find_end idx =
              if idx >= String.size(post_token_lexeme)
              then Left i
              else (case string_indexof_from token post_token_lexeme idx of
                        SOME j =>
                        if is_ending_delim j
                        then
                          (let
                            val rest = String.extract(post_token_lexeme,
                                                      j+token_len,
                                                      NONE);
                          in
                            Right (prefix,
                                   Code (string_trim(String.substring(post_token_lexeme,0,j))),
                                   rest)
                          end)
                        else try_find_end (idx + 1)
                      | NONE => Left i) (* try_find_end (idx + 1)) *)
        in
          try_find_end 0
        end
    end;

fun carve_link s len pos =
    if #"[" <> String.sub(s, pos)
    then NONE
    else
      let
        val prefix = if 0 = pos
                     then NONE
                     else SOME(String.substring(s, 0, pos));
        val lexeme = String.extract(s, pos, NONE);
      in
        case string_indexof "](" lexeme of
           NONE => NONE
         | (SOME i) =>
           let
             val tail = String.extract(lexeme, i+2, NONE);
           in
             (case string_indexof ")" tail of
                  NONE => NONE
               | SOME j => (* (before, link_descr, url, rest) *)
                 SOME (prefix,
                       String.substring(lexeme,1,i-1),
                       String.substring(tail,0, j),
                       String.extract(tail,j+1,NONE)))
           end
      end;

fun try_img s len pos =
    if not (pos + 2 < len andalso
            #"[" = String.sub(s, pos+1))
    then NONE
    else (case carve_link s len (pos + 1) of
              NONE => NONE
            | SOME (NONE, alt, url, rest) =>
              SOME (NONE,
                    Image {img_src=url,
                           img_alt=alt},
                    rest)
            | SOME (SOME txt, alt, url, rest) =>
              SOME (if "!" = txt
                    then NONE
                    else SOME (Text (String.substring(txt,
                                                      0,
                                                      String.size(txt)-1))),
                    Image {img_src=url,
                           img_alt=alt},
                    rest));

fun anchor s pos len =
    let
      val pre = if 0 = pos
                then NONE
                else SOME (Text (String.substring(s,0,pos)));
      val rest_s = String.extract(s,pos+2,NONE);
    in
      case (str_indexof (fn c => #"]" = c) rest_s) of
          NONE => NONE
        | SOME i => SOME (pre,
                          Anchor (String.substring(rest_s,0,i)),
                          String.extract(rest_s,i+1,NONE))
    end;

(* skip_tex : string -> int -> int

Returns the next position, skipping over any TeX code
delimited by '$...$', '$$...$$', '\(...\)', '\[...\]',
'\begin{equation}...\end{equation}', or
'\begin{align}...\end{align}'.

If no such delimiter is found in the next character, then it
should simply increment the position parameter.
*)
fun skip_tex s len pos =
  if pos >= len
  then len
  else if #"$" = String.sub(s,pos)
  then (if len > pos + 1 andalso
           #"$" = String.sub(s,pos+1)
        then (case string_indexof_from "$$" s (pos + 2) of
                  NONE => pos + 2
                | SOME i => i + 2)
        else (case string_indexof_from "$" s (pos + 1) of
                  NONE => pos + 1
                | SOME i => i + 1))
  else if len > pos + 1 andalso
          #"\\" = String.sub(s,pos)
  then (if #"(" = String.sub(s,pos+1)
        then (case string_indexof_from "\\)" s (pos + 1) of
                  NONE => pos + 1
                | SOME i => i + 2)
        else if #"[" = String.sub(s,pos+1)
        then (case string_indexof_from "\\]" s (pos + 1) of
                  NONE => pos + 1
                | SOME i => i + 2)
        else if len > pos + 15 (* size("begin{equation}") *) andalso
                EQUAL = String.compare(
                  String.substring(s, pos+1, 15),
                  "begin{equation}")
        then (case string_indexof_from "\\end{equation}" s (pos + 1) of
                  NONE => pos + 1
                | SOME i => i + 14) (* i + size "\\end{equation}" *)
        else if len > pos + 12 (* size("begin{align}") *) andalso
                EQUAL = String.compare(
                  String.substring(s, pos+1, 12),
                  "begin{align}")
        then (case string_indexof_from "\\end{align}" s (pos + 1) of
                  NONE => pos + 1
                | SOME i => i + 11) (* i + size("\\end{align}") *)
        else pos + 1)
  else pos + 1;

(* interrupted_TeX_index : string -> int -> int -> int -> int option

If there is a TeX delimiter between `start` and `stop`, and if
it is incomplete (i.e., interrupted by `stop`), then return
`SOME idx` for the position of the TeX delimiter.

Otherwise, return NONE.
 *)
fun interrupted_TeX_index s len start stop : int option =
  foldl (fn (pred, NONE) =>
          (case CharVector.findi pred s of
               NONE => NONE
             | SOME(i,_) => if i < stop
                            then (SOME i)
                            else NONE)
        | (_,y) => y)
        NONE
        [(fn (i,c) =>
             i > start andalso
             #"$" = c andalso
             #"\\" <> String.sub(s, i - 1))
        ,(fn (i,c) =>
             #"\\" = c andalso
             start < i andalso i < stop - 1 andalso
             (#"(" = String.sub(s, i + 1) orelse
              #"[" = String.sub(s, i + 1) orelse
              let
                val ss = Substring.extract(s, i+1, NONE)
              in
                Substring.isPrefix "begin{equation}" ss orelse
                Substring.isPrefix "begin{align}" ss
              end))];

(*
This will take "foo **bar** rest" and produce (Bold [Text "bar"], " rest")
We need to handle the "foo " being saved in a Text
*)
fun try_delim s len pos delim delim_len constructor =
  let
    val rest_s = String.extract(s, pos + delim_len, NONE);
    fun skip_to i =
      let
        val lexeme =
          String.substring(rest_s,
                           0,
                           i);
        val lexeme_len = String.size(lexeme);
        val prefix = if 0 = pos
                     then NONE
                     else SOME(String.substring(s, 0, pos));
      in
        SOME (prefix,
              constructor (scan lexeme lexeme_len 0),
              String.extract(rest_s,
                             i + delim_len,
                             NONE))
      end;
  in
    (case string_indexof delim rest_s of
         SOME i => (case interrupted_TeX_index rest_s (len - i) 0 i of
                        SOME j =>
                        (let
                          val k = skip_tex rest_s (len - i) (i + j)
                        in
                          if k = j + 1 then skip_to i
                          else (case string_indexof_from delim rest_s k of
                                    SOME ell => skip_to ell
                                  | NONE => (* false positive?! *) 
                                    NONE
                               )
                        end)
                      | NONE => skip_to i) 
       | NONE => NONE)
     end
and link s len pos = (case carve_link s len pos of
                       NONE => scan s len (pos + 1)
                    | SOME (NONE, descr, url, rest) =>
                      (Link { link_url = url
                            , link_desc=(scan descr
                                               (String.size descr)
                                               0)}) ::
                      (scan rest (String.size rest) 0)
                    | SOME (SOME p, descr, url, rest) =>
                      (Text p) ::
                      (Link {link_desc=(scan descr
                                             (String.size descr)
                                             0)
                            , link_url = url}) ::
                      (scan rest (String.size rest) 0))
and scan s len pos =
  if pos >= len
  then (if 0 = len then [] else [Text s])
  else (case String.sub(s, pos) of
            #"*" =>
                if (pos + 1) < len andalso
                   (#"*" = String.sub(s, pos + 1))
                then (case try_delim s len pos "**" 2 Bold of
                        SOME (NONE, elt, rest) =>
                          elt::(scan rest (String.size rest) 0)
                      | SOME (SOME t, elt, rest) =>
                          (Text t) ::
                          elt ::
                          (scan rest (String.size rest) 0)
                       | NONE => scan s len (pos + 1))
                else scan s len (pos + 1)
          | #"_" => (case try_delim s len pos "_" 1 Emph of
                         SOME (NONE, elt, rest) =>
                           elt::(scan rest (String.size rest) 0)
                       | (SOME (SOME t, elt, rest)) =>
                         (Text t) ::
                         elt ::
                         (scan rest (String.size rest) 0)
                       | NONE => scan s len (pos + 1))
          | #"`" => (case inline_code s len pos of
                      Left i => scan s len (pos + i)
                      | Right(NONE,code,rest) =>
                        code::(scan rest (String.size rest) 0)
                      | Right(SOME t,code,rest) =>
                        (Text t) ::
                        code ::
                        (scan rest (String.size rest) 0))
          | #"[" => if (pos + 1 < len) andalso
                       (#"#" = String.sub(s, pos + 1))
                    then (case (anchor s pos len) of
                            NONE => scan s len (pos + 1)
                          | SOME (NONE,elt,rest) =>
                            elt ::
                            (scan rest (String.size rest) 0)
                          | SOME (SOME txt,elt,rest) => 
                            txt ::
                            elt ::
                            (scan rest (String.size rest) 0))
                    else link s len pos
          | #"!" => (case try_img s len pos of
                         NONE => scan s len (pos + 1)
                       | SOME (NONE,img,rest) =>
                         img :: (scan rest (String.size rest) 0)
                       | SOME (SOME txt,img,rest) =>
                         txt ::
                         img ::
                         (scan rest (String.size rest) 0))
          | _ => scan s len (skip_tex s len pos)
       );

fun parse_inline s = scan s (String.size s) 0;

(* blank_line : substring -> bool

Tests if the contents of the first line of the substring
consists of space characters.
*)
fun blank_line (body : substring) =
  Substring.isEmpty body orelse
  Substring.isPrefix "\n" body orelse
  (Char.isSpace (Substring.sub(body, 0)) andalso
   blank_line (Substring.slice(body, 1, NONE)));

(* next_line : substring -> substring

Returns the substring starting after the first `#"\n"`
character.

If there are none, returns the empty substring.
*)
fun next_line (body : substring) =
  case CharVectorSlice.findi (fn (i,c) => #"\n" = c) body of
      SOME(i,_) => Substring.slice(body, i + 1, NONE)
    | NONE => Substring.slice(body, Substring.size body, NONE); 

(* header : substring -> ('a Block) * substring

Expects one or more hashtags `#`, followed by a space, then
the header title.

More than 6 hashtags "truncated" down to 6 automatically. So
`############ foo` is treated as if it were `###### foo`.

ASSUMES: the body starts with a #"#" character
ENSURES: the result is the header and the following line
*)
fun header (lines : substring) =
  let
    val (header_line, rest) =
        case CharVectorSlice.findi (fn (i,c) => #"\n" = c) lines
         of
            SOME(i,_) => (Substring.slice(lines, 0, SOME i),
                          Substring.slice(lines, i + 1, NONE))
          | NONE => (lines,
                     Substring.slice(lines, Substring.size lines, NONE));
    val header_count =
      case CharVectorSlice.findi
               (fn (_,c) => Char.isSpace c)
               header_line of
            NONE => Substring.size header_line
          | SOME (i,_) => i;
    val txt = Substring.slice(header_line, header_count, NONE);
    val header_txt = parse_inline (Substring.string txt);
  in
    (Heading (Int.min(6, header_count),
              header_txt),
     rest)
  end;

fun substring_trim_lead (s : substring) =
  case CharVectorSlice.findi (fn (i,c) => not (Char.isSpace c))
                             s of
      NONE => s
    | (SOME (i,c)) => Substring.slice(s, i, NONE);

fun substring_trim_tail (s : substring) =
  if Substring.isEmpty s
  then s
  else if Char.isSpace (Substring.sub(s, Substring.size(s) - 1))
  then substring_trim_tail (Substring.slice(s,
                                            0,
                                            SOME (Substring.size s - 1)))
  else s;

fun substring_trim (s : substring) =
  (substring_trim_lead o substring_trim_tail) s;

(* is_example : substring -> bool

ASSUME: line has at most one `#"\n"` character, at the very
        end of it
ENSURES: result is true if "{...,example,...}" appears in line
                        OR "{...,example=v,...}" appears
                           and `v` is not "false"
         result is false otherwise.
*)
fun is_example (line : substring) =
  let
    fun indexof (f : char -> bool) (s : substring) =
      (case CharVectorSlice.findi (fn (i,c) => f c) s of
           NONE => NONE
         | SOME (i,c) => SOME i);
    fun get i j =
      case (List.filter (fn token =>
                            Substring.isPrefix "example" token)
                        (map substring_trim
                             (Substring.tokens (fn c => #"," = c)
                                               (Substring.slice(line,i,j)))))
       of
          [] => false
        | entries => List.exists
                         (fn e =>
                             let
                               val entry = Substring.string e
                             in
                               ("example" = entry) orelse
                               (case (map string_trim
                                          (String.fields
                                               (fn c => #"=" = c)
                                               entry))
                                 of
                                    ["example", rhs] => ("false" <> rhs)
                                  | _ => false)
                             end)
                         entries;
  in
    not (Substring.isPrefix "\n" line) andalso
    case indexof (fn c => #"\n" = c) line of
        NONE => false
      | k =>
        let
          val s = Substring.slice(line, 0, k)
        in
          (case (indexof (fn c => #"{" = c) s,
                 indexof (fn c => #"}" = c) s) of
               (SOME i, SOME j) => (get (i+1) (SOME (j - i - 1)))
             | _ => false)
        end
  end;

(* pre_meta : substring -> string option * bool

Returns the name of the language, and if the code chunk is an
example or not.

If the language ends in an asterisk, then this is interpreted
as always an example.

ASSUMES: body starts with three backticks
ENSURES: result is (NONE, false) if body starts with blank_line
 result is (lang, false) if body looks like "```lang\n..."
 result is (lang, true) if body looks like "```lang*\n..."
                           OR "```lang {...,example,...}\n..."
                           OR "```lang {...,example=v,...}\n..."
                              and `v` is not "false"
 *)
fun pre_meta (body : substring) =
  let
    fun trim_ast lang =
      Substring.string
          (if Substring.isSuffix "*" lang
           then Substring.slice(lang,
                                0,
                                SOME((Substring.size lang)-1))
           else lang);
    fun ex_status lang = Substring.isSuffix "*" lang;
    (* body always looks like "```...\n", so skip the
       backticks and jump to first non whitespace character.
     *)
    val line = (case CharVectorSlice.findi
                        (fn (i,c) =>
                            i > 2 andalso
                            (not(Char.isSpace c) orelse
                             #"\n" = c))
                        body of
                   NONE => Substring.slice(body,
                                           Substring.size body,
                                           NONE)
                 | (SOME (i,_)) => Substring.slice(body, i, NONE));
  in
    if blank_line line
    then (NONE, false)
    else (case CharVectorSlice.findi
                   (fn (i,c) => Char.isSpace c)
                   line of
              NONE => (NONE, false)
            | SOME(i,_) =>
              let
                   val lang = Substring.slice(line, 0, SOME i)
                 in
                   (SOME(trim_ast lang),
                    (ex_status lang) orelse
                    (is_example line))
                 end)
  end;

(* pre : substring -> string Block * substring

CommonMark insists unclosed code blocks are closed by the end
of the document.
 *)
fun pre (body : substring) =
  let
    val (lang, is_ex) = pre_meta body;
    val b1 = next_line body;
    val (code_body,rest) =
      (case CharVectorSlice.findi
                (fn (i,c) =>
                    i + 2 < Substring.size b1 andalso
                    (0 = i orelse
                     (i > 0 andalso
                      #"\n" = Substring.sub(b1, i - 1))) andalso
                    #"`" = c andalso
                    #"`" = Substring.sub(b1, i + 1) andalso
                    #"`" = Substring.sub(b1, i + 2))
                b1 of
           NONE => (b1,
                    Substring.slice(b1, Substring.size b1, NONE))
         | SOME(i,_) => (Substring.slice(b1, 0, SOME (i-1)),
                         Substring.slice(b1,
                                         Int.min(i + 3,
                                                 Substring.size b1),
                                         NONE)));
  in
    (Pre { code = Substring.string code_body
         , language = lang
         , is_example = is_ex
         },
     rest)
  end;

(* NB: items start look like "[digit]+. .*",
   in particular it need not be sequential! *)
fun starts_olist (body : substring) =
  Substring.size body > 2 andalso
  Char.isDigit (Substring.sub(body,0)) andalso
  (#"." = Substring.sub(body,1) orelse
   starts_olist (Substring.slice(body, 1, NONE)));

(* given the body to a paragraph, parse it to a list of inline
fun par_body (lines : string list) =; *)
fun starts_block (body : substring) =
    blank_line body orelse
    starts_olist body orelse
    List.exists (fn tok => Substring.isPrefix tok body)
                ["```",
                 ">",
                 "- ",
                 "+ "];

(* par : substring -> 'a Block * substring

ASSUMES: body starts with the first line of the paragraph
ENSURES: result is (paragraph, rest)
*)
fun par (body : substring) =
  let
    val (ps, rest) =
      case CharVectorSlice.findi
               (fn (i,c) =>
                   #"\n" = c andalso
                   i < Substring.size body andalso
                   starts_block (Substring.slice(body, i+1, NONE)))
               body of
          NONE => (body,
                   Substring.slice(body,
                                   Substring.size body,
                                   NONE))
        | SOME (i,_) => (let
                          val idx =
                            Int.min(Substring.size(body),i+1);
                        in (Substring.slice(body, 0, SOME(idx)),
                            Substring.slice(body, idx, NONE))
                        end);
  in
    (Par (parse_inline (Substring.string ps)),
     rest)
  end;

(* blockquote : substring -> 'a Block * substring

*)
fun blockquote (body : substring) =
  let
    fun is_not_blockquote (i,c) =
      (0 = i orelse
       #"\n" = Substring.sub(body, i-1)) andalso
      #">" <> c;
    fun trim_bq (line : substring) =
        if Substring.isPrefix "> " line
        then Substring.slice(line,2,NONE)
     (* else we have ">" for separating paragraphs in blockquote *)
        else Substring.slice(line,1,NONE);
    val (quote_lines, rest) =
        case CharVectorSlice.findi is_not_blockquote body of
            SOME(i,_) => (Substring.slice(body, 0, SOME i),
                          Substring.slice(body, i, NONE))
          | NONE => (body,
                     Substring.slice(body,
                                     Substring.size body,
                                     NONE));
  in
    (Quote (parse_block_string
                (((Substring.concatWith "\n") o
                  (map trim_bq) o
                  (Substring.tokens (fn c => #"\n" = c)))
                     quote_lines)),
     rest)
  end

(* parse_list : substring ->
                (substring -> bool) ->
                ((Block list) list) * substring

NOTE: paragraphs in a list MUST be properly indented. Blank
lines are considered "terminators" for lists.
 *)
(* XXX: slow, this could be optimized to improve performance *)
and parse_list (lines : substring)
               (starts_item : substring -> bool) = 
  let
    (* is_item : substring -> bool *)
    fun is_item (line : substring) =
      (Substring.size(line) > 0 andalso
       Substring.sub(line, 0) <> #"\n" andalso
       (starts_item line orelse
        (* indented and nonblank line
           or else followed by a line in an item *)
        (Substring.isPrefix "  " line)));
    (*
          (Substring.isPrefix "  " line andalso (* XXX: check indentation? *)
           (not (blank_line line) orelse
            is_item (next_line line)))));
    *)
    (* trim_item : substring -> string *)
    (* XXX: slow, this could be optimized to improve performance *)
    fun trim_item (lines : substring) =
        let
          val indent_size =
            (case CharVectorSlice.findi
                      (fn (i,c) => Char.isSpace c)
                      lines of
                   SOME (i,_) => 1 + i
                 | NONE => 3);
          val indent = implode(List.tabulate (indent_size,
                                              fn _ => #" "));
          fun trim_line (line : substring) =
              if Substring.size(line) >= indent_size
              then Substring.slice(line, indent_size, NONE)
              else line;
        in
          (((Substring.concatWith "\n") o
            (map trim_line) o
            (Substring.tokens (fn c => #"\n" = c)))
               lines)
        end;
    (* separate the list from the rest of the Markdown *)
    fun find_rest ls = if Substring.isEmpty ls then ls
                       else if is_item ls
                       then find_rest (next_line ls)
                       else ls;
    val (item_lines, rest) =
      case CharVectorSlice.findi (fn (i,c) =>
                                     #"\n" = c andalso
                                     i+1 <= Substring.size lines andalso
                                     ((not o is_item)
                                          (Substring.slice(lines,i+1,NONE))))
                                 lines of
          NONE => (lines,
                   Substring.slice(lines, Substring.size(lines), NONE))
        | (SOME(i,_)) =>
          (Substring.slice(lines,0,SOME(i)),
           Substring.slice(lines,i,NONE));
    (* form the list of lines in each item *)
    (* part_items : substring -> substring list -> substring list *)
    (* XXX: slow, this could be optimized to improve performance *)
    fun part_items ls acc =
      if Substring.isEmpty ls
      then rev acc
      else (case CharVectorSlice.findi
                     (fn (i,c) => i > 0 andalso
                                  Substring.sub(ls, i-1) = #"\n" andalso
                                  starts_item
                                      (Substring.slice(ls,i,NONE)))
                     ls of
                (SOME(i,_)) => part_items (Substring.slice(ls,i,NONE))
                                          ((Substring.slice(ls,0,SOME i))::acc)
              | NONE => rev (ls::acc));
    (* recursively trim then parse each item for its block
       structure *)
    val items = map (fn (item_lines : substring) =>
                        parse_block_string (trim_item item_lines))
                    (part_items item_lines []);
  in
    (* process and trim items *)
    (items, rest)
  end

and ulist (body : substring) (token : char) =
  let
    fun starts_item (line : substring) =
        (Substring.size(line) > 1 andalso
         token = Substring.sub(line, 0) andalso
         #" " = Substring.sub(line,1));
    val (items, rest) = parse_list body starts_item;
  in
    (UList items, rest)
  end

and olist (body : substring) =
  let
    val (items, rest) = parse_list body starts_olist;
  in
    (OList items, rest)
  end
(* parse_block : substring -> string Block list *)
and parse_block body =
  let
    fun parse_iter acc (s : substring) =
      if Substring.isEmpty s
      then rev acc
      else if blank_line s
      then parse_iter acc (next_line s)
      else let
        val (block, rest) =
          if Substring.isPrefix "#" s
          then header s
          else if Substring.isPrefix ">" s
          then blockquote s
          else if Substring.isPrefix "```" s
          then pre s
          else if Substring.isPrefix "- " s
          then ulist s #"-"
          else if Substring.isPrefix "+ " s
          then ulist s #"+"
          else if starts_olist s
          then olist s
          else par s;
      in
        parse_iter (block::acc) rest
      end;
  in
    parse_iter [] body
  end
and parse_block_string (s : string) =
    parse_block (Substring.full s);

(* end of metadata : string -> int *)
fun end_of_metadata (s : string) =
    string_indexof_from "\n---" s 4;

(* starts_metadata : string -> bool *)
fun starts_metadata (s : string) =
    String.isPrefix "---\n" s;

(* extract_metadata : string -> ((string, string) list)*substring *)
fun extract_metadata (s : string) =
    if not (starts_metadata s)
    then ([], Substring.full s)
    else (case end_of_metadata s of
              NONE => ([], Substring.full s)
            | (SOME idx) =>
              let
                val header = String.substring(s,4,idx-3);
                (* ASSUME: each "key: value" takes place on
                   exactly one line *)
                val lines = (String.tokens (fn c => #"\n" = c)
                                           header);
                (* Each line looks like "key: value" *)
                fun extract line =
                  case str_indexof (fn c => #":" = c) line of
                      NONE => NONE
                    | (SOME idx) => SOME (string_trim
                                              (String.substring(line,0,idx)),
                                          string_trim
                                              (String.extract(line,idx+1,NONE)));
                val meta = foldr (fn (line,acc) =>
                                     (case extract line of
                                          SOME c => c::acc
                                        | _ => acc))
                                 []
                                 lines;
                val i = Int.min(idx+4,
                                String.size s);
                val body = Substring.extract(s, i, NONE);
              in
                (meta, body)
              end);


(* parse : string -> ((string * string) list) * (string Block list) *)
fun parse (s : string) =
  let
    val (metadata,body) = extract_metadata s
  in
    (metadata, parse_block body)
  end;

end;
