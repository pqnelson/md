

(* escape_html : string -> string

Escapes the usual HTML entities, plus ones which might
accidentally be captured as TeX start/end delimiters. I have
intentionally added `&#xfeff;` after dollar signs and back
slashes to interrupt things.

**NOT idempotent.**

This is intended to be the default which a syntax highlighter
should replace.
*)
val escape_html =
  String.translate (fn #"&" => "&amp;"
                   | #"<" => "&lt;"
                   | #">" => "&gt;"
                   | #"\"" => "&quot;"
                   | #"'" => "&apos;"
                   | #"$" =>  "&dollar;&#xfeff;"
                   | #"\\" => "&bsol;&#xfeff;" 
                   | c => String.str c);

(* pprint : string -> string

Takes a text string which will appear in part of a non-code
environment, and replace certain patterns with their intended
HTML entity (like "---" with an em-dash).

This is idempotent.
*)
fun pprint s =
    foldl (fn ((raw,replace),acc) =>
              string_replace_all raw acc replace)
          s
          (* order matters, tries from left to right *)
          [ ("---", "&mdash;")
          , ("--", "&ndash;")
          , ("...", "&hellip;")
          ];

structure Html5 : EMITTER = struct
fun emit_inline elt =
    case elt of
        Text x => pprint x
      | Emph x => ("<i>" ^
                   (concat (map emit_inline x)) ^
                   "</i>")
      | Bold x => ("<b>" ^
                   (concat (map emit_inline x)) ^
                   "</b>")
      | Code x => ("<code>" ^ (escape_html x) ^ "</code>")
      | Link {link_url,link_desc} =>
        ("<a href=\"" ^
         (if is_md link_url
          then md_to_html link_url
          else link_url)^
         "\">" ^
         (concat (map emit_inline link_desc)) ^
         "</a>")
      | Anchor x => ("<a id=\""^x^"\"></a>")
      | Image {img_src, img_alt} => ("<img src=\"" ^
                                     img_src ^
                                     "\" alt=\"" ^
                                     img_alt ^
                                     "\" />");

(*
emit_block : ('a -> string) -> 'a Block -> string

...where (syntax_highlight : 'a -> string) is the first
argument.

Note that `<pre>` should NOT have a newline separating it from
the code.

REQUIRES: `syntax_highlight` MUST perform HTML escaping for
the usual entities (#"<", #">", #"&", #"\"", etc.). 
*)
fun emit_block syntax_highlight block =
    case block of
        Par elts =>
        (case elts of
             (* Hack for div blocks *)
             [(elt as (Text e))] =>
                 if String.isPrefix "<div" e orelse
                    String.isPrefix "</div>" e
                 then e
                 else ("\n<p>\n" ^
                       (emit_inline elt) ^
                       "\n</p>\n")
           | _ =>  ("\n<p>\n" ^
                    (concat (map emit_inline elts)) ^
                    "\n</p>\n"))
      | (Pre {code,language=NONE,...}) => ("\n<pre>"^
                                           (syntax_highlight code) ^
                                           "</pre>\n")
      | Pre {code,language=SOME lang,is_example} =>
        ("\n<pre data-lang=\"" ^
         lang ^
         "\" class=\"" ^
         (if is_example then "example" else "src") ^
         "\">" ^
         (syntax_highlight code) ^
         "</pre>\n")
      | Heading (lvl, title) =>
        ("\n<h" ^
         (Int.toString lvl) ^
         ">" ^
         ((string_trim o concat) (map emit_inline title))^
         "</h" ^
         (Int.toString lvl) ^
         ">\n")
      | Quote blocks =>
        ("\n<blockquote>\n"^
         (concat
              (map (emit_block syntax_highlight) blocks))^
         "\n</blockquote>\n")
      | UList items =>
        ("\n<ul>" ^
         (concat
              (map (fn item =>
                       ("<li>" ^
                        (concat
                             (map (emit_block syntax_highlight)
                                  item)) ^
                        "</li>\n"))
                   items) ^
          "</ul>\n"))
      | OList items =>
        ("\n<ol>" ^
         (concat
              (map (fn item =>
                       ("<li>" ^
                        (concat
                             (map (emit_block syntax_highlight)
                                  item)) ^
                        "</li>\n"))
                   items) ^
          "</ol>\n"));

fun emit syntax_highlight blocks =
    concat (map (emit_block syntax_highlight) blocks);
end;

(* ...because I keep forgetting the *necessary* elements of
HTML5... *)
val generic_header = concat
  [ "<!doctype html>\n"
  , "<html lang=\"en-US\">\n"
  , "<head>\n"
  , "<meta charset=\"utf-8\" />\n"
  , "<meta name=\"viewport\" content=\"width=device-width\" />"
  , "\n"
  ];

fun add_title metadata header =
    case (List.filter (fn (x,y) => "title" = x) metadata) of
        [] => header
      | (_,title)::_ => "<title>"^(title)^"</title>\n"^header;

fun html5 (s : string) (header : string) (footer : string) =
    let
      val (metadata, body) = Md.parse s;
    in
      generic_header ^
      (add_title metadata header) ^
      "\n</head>\n<body>\n" ^
      "<main>\n" ^
      (Html5.emit escape_html body) ^
      "\n" ^
      footer ^
      "</main>\n" ^
      "\n</body>\n" ^
      "</html>\n"
    end;
        
