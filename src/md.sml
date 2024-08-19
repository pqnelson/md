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
        in (case string_indexof token post_token_lexeme of
                SOME j =>
                let
                  val rest = String.extract(post_token_lexeme,j+i,NONE);
                in
                  Right (prefix,
                         Code (String.substring(post_token_lexeme,0,j)),
                         rest)
                end
              | NONE => Left i)
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

(*
This will take "foo **bar** rest" and produce (Bold [Text "bar"], " rest")
We need to handle the "foo " being saved in a Text
*)
fun try_delim s len pos delim delim_len constructor =
  let
    val rest_s = String.extract(s, pos + delim_len, NONE);
  in
    (case string_indexof delim rest_s of
       SOME i => (let
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
                 end)
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
  then [Text s]
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
          | _ => scan s len (pos + 1) 
       );

fun parse_inline s = scan s (String.size s) 0;


fun blank_line (line : string) =
    CharVector.all Char.isSpace line;

fun header (lines : string list) =
  let
    val (header_lines, rest) =
        case list_indexof blank_line lines of
            (SOME i) => (List.take (lines, i+1),
                         List.drop (lines, i+1))
          | NONE => (lines, []);
    val lead_line = hd header_lines;
    val header_count =
        case str_indexof Char.isSpace lead_line of
            NONE => String.size lead_line
          | SOME i => i;
    val ls =
        (String.extract(lead_line,header_count,NONE)) ::
        (tl header_lines);
    val txt = String.concatWith "\n" ls;
    val header_txt = parse_inline txt;
  in
    (Heading (Int.min(6, header_count),
              header_txt),
     rest)
  end;

fun pre (lines : string list) =
  let
    val meta_raw = String.extract(hd lines, 3, NONE);
    val meta = if blank_line meta_raw
               then NONE
               else SOME meta_raw
    fun ends_pre l = String.isPrefix "```" l;
    val (body,rest) = (case list_indexof ends_pre (tl lines) of
                           NONE => (lines, [])
                         | SOME i => (List.take(tl lines, i),
                                      List.drop(tl lines, i+1)));
  in
    (Pre (String.concatWith "\n" body, meta),
     rest)
  end;

(* NB: items start look like "[digit]+. .*",
   in particular it need not be sequential! *)
fun starts_olist (line : string) =
  case str_indexof (fn c => #"." = c) line of
      NONE => false
    | SOME i => (String.size(line) > i) andalso
                (CharVector.all
                   (fn c => Char.isDigit c)
                   (String.substring(line, 0, i))) andalso
                Char.isSpace(String.sub(line,i+1));
                            
(* given the body to a paragraph, parse it to a list of inline
fun par_body (lines : string list) =; *)
fun starts_block (line : string) =
    blank_line line orelse
    starts_olist line orelse
    List.exists (fn tok => String.isPrefix tok line)
                ["```",
                 ">",
                 "- ",
                 "+ "];

fun par (lines : string list) =
  let
    val (ps, rest) = case list_indexof starts_block lines of
                         NONE => (lines, [])
                       | SOME i => (List.take(lines, i),
                                    List.drop(lines, i)); 
  in
    (Par (parse_inline (String.concatWith "\n" ps)),
     rest)
  end;

fun blockquote (lines : string list) =
  let
    fun is_blockquote (s : string) = (String.size(s) > 0 andalso
                                      #">" = String.sub(s,0));
    fun trim_bq (s : string) =
        if String.isPrefix "> " s
        then String.extract(s,2,NONE)
     (* else we have ">" for separating paragraphs in blockquote *)
        else String.extract(s,1,NONE);
    val (quote_lines, rest) =
        case list_indexof (not o is_blockquote) lines of
            SOME i => ((List.take(lines, i)),
                       List.drop(lines, i))
          | NONE => (lines, []);
  in
    (Quote (parse_block (map trim_bq quote_lines)),
     rest)
  end

(* parse_list : string list ->
                (string -> bool) ->
                ((Block list) list)

NOTE: paragraphs in a list MUST be properly indented. Blank
lines are considered "terminators" for lists.
 *)
and parse_list (lines : string list)
               (starts_item : string -> bool) = 
  let
    fun is_item (line : string) =
        (String.size(line) > 0 andalso
         (starts_item line orelse
          String.isPrefix "  " line)); (* XXX: check indentation? *)
    fun trim_item (lines : string list) =
        let
          val indent_size =
              (case str_indexof Char.isSpace (hd lines) of
                   SOME i => 1 + i
                 | NONE => 3);
          val indent = implode(List.tabulate (indent_size,
                                              fn _ => #" "));
          fun trim_line (line : string) =
              if String.size(line) >= indent_size
              then String.extract(line, indent_size, NONE)
              else line;
        in
          map trim_line lines
        end;
    (* separate the list from the rest of the Markdown *)
    val (item_lines, rest) =
        case list_indexof (not o is_item) lines of
            SOME i => (List.take (lines, i+1),
                       List.drop (lines, i+1))
          | NONE => (lines, []);
    (* form the list of lines in each item *)
    (* part_items : string list -> string list list *)
    fun part_items [] acc = rev acc
      | part_items (ls as h::t) acc =
        (case list_indexof starts_item t of
             SOME i => part_items (List.drop(t, i))
                                  ((List.take(ls, i+1))::acc)
           | NONE => rev (ls::acc));
    (* recursively trim then parse each item for its block
       structure *)
    val items = map (fn (item_lines : string list) =>
                        parse_block (trim_item item_lines))
                    (part_items item_lines []);
  in
    (* process and trim items *)
    (items, rest)
  end

and ulist (lines : string list) (token : char) =
  let
    fun starts_item (line : string) =
        (String.size(line) > 1 andalso
         token = String.sub(line, 0) andalso
         #" " = String.sub(line,1));
    val (items, rest) = parse_list lines starts_item;
  in
    (UList items, rest)
  end

and olist (lines : string list) =
  let
    val (items, rest) = parse_list lines starts_olist;
  in
    (OList items, rest)
  end
(* parse_block : string list -> string Block list *)
and parse_block lines =
  let
    fun parse_iter acc =
        (fn [] => rev acc
          | (lines as line::_) =>
            if blank_line line
            then parse_iter acc (tl lines)
            else 
              let
                val (block, rest) =
                    if String.isPrefix "#" line
                    then header lines
                    else if String.isPrefix ">" line
                    then blockquote lines
                    else if String.isPrefix "```" line
                    then pre lines
                    else if String.isPrefix "- " line
                    then ulist lines #"-"
                    else if String.isPrefix "+ " line
                    then ulist lines #"+"
                    else if starts_olist line
                    then olist lines
                    else par lines;
              in
                parse_iter (block::acc) rest
              end)
  in
    parse_iter [] lines
  end;

(* parse : string -> string Block list *)
fun parse (s : string) =
  let
    val lines =  (String.fields (fn c => #"\n" = c) s)
  in
    parse_block lines
  end;

end;
