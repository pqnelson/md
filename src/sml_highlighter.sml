structure SmlHighlighter = struct
  
datatype 'a Trie = Null
                 | Leaf of 'a
                 | Branch of 'a * 'a Trie * 'a Trie;

fun height (Null) = 0
  | height (Leaf _) = 1
  | height (Branch (_,l,r)) = 1 + Int.max(height l, height r); 

fun min_height (Null) = 0
  | min_height (Leaf _) = 1
  | min_height (Branch (_,l,r)) = 1 + Int.min(min_height l, min_height r); 

local
  fun mk_trie (s : string) =
    if 0 >= size s
    then Null
    else if 1 = size s
    then Leaf (String.sub(s, 0))
    else let
      val n = (size s) div 2;
      val left = mk_trie (String.substring(s, 0, n));
      val right = mk_trie (String.extract(s, n + 1, NONE));
    in
      Branch(String.sub(s,n), left, right)
    end;
in
fun mk_trie_tokenizer (symbolic_chars_str : string) =
  let
    val trie = mk_trie symbolic_chars_str;
    fun lookup_iter (needle : char) (Null) = false
      | lookup_iter needle (Leaf c) = (needle = c)
      | lookup_iter needle (Branch(c,l,r)) =
        case Char.compare(needle,c) of
            LESS => lookup_iter needle l
          | EQUAL => true 
          | GREATER => lookup_iter needle r;
    (* these two functions are useful for determining
     statistical properties of the trie and the linear
     alternative. *)
    fun count_lookup n needle (Null) = n
      | count_lookup n needle (Leaf c) = n + 1
      | count_lookup n needle (Branch(c,l,r)) =
        case Char.compare(needle, c) of
            LESS => count_lookup (n + 1) needle l
          | EQUAL => n
          | GREATER => count_lookup (n + 1) needle r;
    
    fun count_lookup2 n needle =
      if n >= size symbolic_chars_str
      then n
      else if String.sub(symbolic_chars_str, n) = needle
      then n + 1
      else count_lookup2 (n + 1) needle;
    (* is_symbolic_identifier_char : char -> bool *)
    fun is_symbolic_identifier_char (c : char) =
      lookup_iter c trie;
    (* symbolic_identifier : string -> int -> string * int
REQUIRES: true
ENSURES: (s',stop) = result implies
           s' = String.substring(s, start, stop - start) andalso
           stop > start andalso
           not (is_symbolic_identifier_char s stop) andalso
           List.all is_symbolic_identifier_char (explode s');
     *)
    fun symbolic_identifier s start =
      let
        fun iter cur =
          if cur >= size s
          then (String.extract(s,start,NONE), size s)
          else if is_symbolic_identifier_char (String.sub(s, cur))
          then iter (cur + 1)
          else (String.substring(s,start,cur-start),cur);
      in
        if start < 0
        then symbolic_identifier s 0
        else iter start
      end;
  in
    (is_symbolic_identifier_char, symbolic_identifier)
  end
end;

val (is_symbolic_identifier_char, symbolic_identifier) =
  mk_trie_tokenizer "!#$%&*+-/:<=>?@\\^`|~";

fun starts_symbolic_identifier s start =
  if start < 0 then starts_symbolic_identifier s 0
  else start < size s andalso
       is_symbolic_identifier_char (String.sub(s, start));

fun starts_alphanum_id s start =
  if start < 0 then starts_alphanum_id s 0
  else start < size s andalso
       (Char.isAlpha (String.sub(s, start)) orelse
        #"'" = String.sub(s,start));

local
  fun is_alphanum_id_char (c : char) =
    Char.isAlphaNum c orelse
    #"_" = c orelse
    #"'" = c;
  fun alphanum_id_body s start pos =
    if pos >= size s
    then (String.extract(s, start, NONE), size s)
    else if is_alphanum_id_char (String.sub(s,pos))
    then alphanum_id_body s start (pos + 1)
    else (String.substring(s, start, pos-start), pos);
in
fun alphanum_identifier (s : string) (start : int) =
  if start < 0
  then alphanum_identifier s 0
  else if size s <= start
  then ("", size s)
  else if starts_alphanum_id s start
  then alphanum_id_body s start start
  else ("", start)
end;

fun is_hexa_int s start =
  start < size s andalso
  if #"~" = String.sub(s,start) then is_hexa_int s (start + 1)
  else (start + 1 < size s andalso
        #"0" = String.sub(s, start) andalso
        #"x" = String.sub(s, start + 1));

fun is_word s start =
  1 + start < size s andalso
  #"0" = String.sub(s, start) andalso
  #"w" = String.sub(s, start + 1);

fun is_hexa_word s start =
  2 + start < size s andalso
  #"0" = String.sub(s, start) andalso
  #"w" = String.sub(s, start + 1) andalso
  #"x" = String.sub(s, start + 2);

fun is_int s start =
  start < size s andalso
  if #"~" = String.sub(s, start)
  then (start + 1 < size s andalso
        Char.isDigit (String.sub(s, start + 1)))
  else Char.isDigit (String.sub(s, start));

(* most general test if this starts a number, or something
like a number *)
fun starts_number s start =
  start < size s andalso
  ((#"~" = String.sub(s, start) andalso
    start + 1 < size s andalso
    Char.isHexDigit(String.sub(s, start + 1))) orelse
   Char.isHexDigit(String.sub(s, start)));

local
  fun decimal_int s start pos =
    if pos >= size s
    then (String.extract(s, start, NONE), size s - start)
    else if Char.isDigit (String.sub(s, pos))
    then decimal_int s start (pos + 1)
    else (String.substring(s, start, pos - start), pos);
  fun hexadecimal_int s start pos =
    if pos >= size s
    then (String.extract(s, start, NONE), size s - start)
    else if Char.isHexDigit (String.sub(s, pos))
    then hexadecimal_int s start (pos + 1)
    else (String.substring(s, start, pos - start), pos);
in
fun number_token s start =
  if is_int s start
  then decimal_int s start (start +
                            (if #"~" = String.sub(s,start)
                            then 2
                            else 1))
  else if is_hexa_word s start
  then hexadecimal_int s start (start + 3)
  else if is_word s start
  then decimal_int s start (start + 2)
  else if is_hexa_int s start
  then hexadecimal_int s start (start +
                                (if #"~" = String.sub(s,start)
                                 then 3
                                 else 2))
  else ("", start)
end;

fun starts_string s start =
  if start <= 0
  then String.isPrefix "\"" s
  else start < size s andalso
       #"\"" = String.sub(s, start);

(* Assert.!! (ends_string "\"foo\" is a string" 4)
 Assert.!! not (ends_string "\"foo\" is a string" 3)
 *)
fun ends_string s pos =
  pos > 0 andalso pos < size s andalso
  #"\"" = String.sub(s, pos) andalso
  #"\\" <> String.sub(s, pos - 1);

local
  fun iter s start cur =
    if cur >= size s
    then (String.extract(s, start, NONE), size s)
    else if ends_string s cur
    then (String.substring(s, start, 1 + cur - start), 1 + cur)
    else iter s start (cur + 1);
in
fun string_token s start =
  if start < 0
  then string_token s 0
  else if start < size s andalso starts_string s start
  then iter s start (start + 1)
  else ("", start)
end;

fun starts_comment s start =
  if start <= 0
  then String.isPrefix "(*" s
  else start + 1 < size s andalso
       #"(" = String.sub(s, start) andalso
       #"*" = String.sub(s, start + 1);

fun ends_comment s start =
  if start <= 0
  then String.isPrefix "*)" s
  else start + 1 < size s andalso
       #"*" = String.sub(s, start) andalso
       #")" = String.sub(s, start + 1);

(* comment : string -> int -> string * int *)
local
  fun iter s start depth pos =
    if depth <= 0
    then (String.substring(s,start,pos - start), pos)
    else if pos >= size s
    then (String.extract(s, start, NONE), size s)
    else if starts_comment s pos
    then iter s start (depth + 1) (pos + 2)
    else if ends_comment s pos
    then iter s start (depth - 1) (pos + 2)
    else iter s start depth (pos + 1);
in
fun comment s start =
  if start < 0
  then comment s 0
  else if not (starts_comment s start)
  then ("", start)
  else iter s start 1 (start + 2)
end;

fun starts_nonid_reserved s start =
  (start < size s andalso
   List.exists (fn c => c = String.sub(s, start))
               (explode "()[]{},;_")) orelse
  (start + 3 < size s andalso
   #"." = String.sub(s, start) andalso
   #"." = String.sub(s, start + 1) andalso
   #"." = String.sub(s, start + 2));

fun nonid_reserved s start =
  if start < 0
  then nonid_reserved s 0
  else if (start < size s andalso
           List.exists (fn c => c = String.sub(s, start))
                       (explode "()[]{},;_"))
  then (String.extract(s, start, SOME 1), start + 1)
  else if (start + 3 < size s andalso
           #"." = String.sub(s, start) andalso
           #"." = String.sub(s, start + 1) andalso
           #"." = String.sub(s, start + 2))
  then ("...", start + 3)
  else ("", start);

fun starts_whitespace s start =
  if start < 0 then starts_whitespace s 0
  else start < size s andalso
       Char.isSpace(String.sub(s,start));

local
  fun iter s start cur =
    if size s <= cur
    then (String.substring(s, start, size s - start), size s)
    else if Char.isSpace(String.sub(s, cur))
    then iter s start (cur + 1)
    else (String.substring(s, start, cur - start), cur);
in
fun whitespace s start =
  if start < 0 then whitespace s 0
  else iter s start start
end;

local
  fun mk_trie (vs : string VectorSlice.slice) =
    case VectorSlice.length vs of
        0 => Null
      | 1 => Leaf (VectorSlice.sub(vs,0))
      | len => 
        let
          val n = len div 2;
          val x = VectorSlice.sub(vs,n);
          val left = mk_trie (VectorSlice.subslice(vs, 0, SOME n));
          val right = mk_trie (VectorSlice.subslice(vs, n + 1, NONE));
        in
          Branch (x, left, right)
        end;

  fun count_lookup n needle (Null) = n
      | count_lookup n needle (Leaf c) = n + 1
      | count_lookup n needle (Branch(c,l,r)) =
        case String.compare(needle, c) of
            LESS => count_lookup (n + 1) needle l
          | EQUAL => n
          | GREATER => count_lookup (n + 1) needle r;
  fun lookup_iter needle (Null : string Trie) = false
    | lookup_iter needle (Leaf c) = (needle = c)
    | lookup_iter needle (Branch(c,l,r)) =
      case String.compare(needle,c) of
          LESS => lookup_iter needle l
        | EQUAL => true 
        | GREATER => lookup_iter needle r;
  val kws = VectorSlice.full (Vector.fromList ["abstype", "and", "andalso", "as", "case", "datatype", "do", "else", "end", "eqtype", "exception", "fn", "fun", "functor", "handle", "if", "in", "include", "infix", "infixr", "let", "local", "nonfix", "of", "op", "open", "orelse", "raise", "rec", "sharing", "sig", "signature", "struct", "structure", "then", "type", "val", "where", "while", "with", "withtype"]);
  val trie = mk_trie kws;
in
fun is_reserved_kw s =
  lookup_iter s trie
end;

datatype Token = Identifier of string
               | Reserved of string
               | Number of string
               | String of string
               | Comment of string
               | Whitespace of string
               | Punct of string
               | Raw of string;

(* tokenize : string -> Token list *)
local
  fun tokenize_iter s acc start =
    if start >= size s
    then rev acc
    else if starts_whitespace s start
    then let val (token, next) = whitespace s start
         in tokenize_iter s ((Whitespace token)::acc) next
         end
    else if starts_alphanum_id s start
    then let val (token, next) = alphanum_identifier s start
         in tokenize_iter s ((if is_reserved_kw token
                              then Reserved token
                              else Identifier token)::acc) next
         end
    else if starts_symbolic_identifier s start
    then let val (token, next) = symbolic_identifier s start
         in tokenize_iter s ((Identifier token)::acc) next
         end
    else if starts_comment s start
    then let val (token, next) = comment s start
         in tokenize_iter s ((Comment token)::acc) next
         end
    else if starts_string s start
    then let val (token, next) = string_token s start
         in tokenize_iter s ((String token)::acc) next
         end
    else if starts_number s start
    then let val (token, next) = number_token s start
         in tokenize_iter s ((Number token)::acc) next
         end
    else if starts_nonid_reserved s start
    then let val (token, next) = nonid_reserved s start
         in tokenize_iter s ((Punct token)::acc) next
         end
    else (* we should not be here, but since we are, just treat
it as a 1-character mystery token *)
      tokenize_iter s
                    ((Raw (String.substring(s, start, 1)))::acc)
                    (start + 1);
in
fun tokenize s = tokenize_iter s [] 0
end;

local
  val escape_html =
    String.translate (fn #"&" => "&amp;"
                     | #"<" => "&lt;"
                     | #">" => "&gt;"
                     | #"\"" => "&quot;"
                     | #"'" => "&apos;"
                     | #"$" =>  "&dollar;&#xfeff;"
                     | #"\\" => "&bsol;&#xfeff;" 
                     | c => String.str c);
  fun span klass content = ("<span class=\"" ^
                            klass ^
                            "\">" ^
                            (escape_html content) ^
                            "</span>");
  fun token_to_html5 (Identifier x) = span "n" x
    | token_to_html5 (Reserved x) = span "k" x
    | token_to_html5 (Number x) = span "m" x
    | token_to_html5 (String x) = span "s" x
    | token_to_html5 (Comment x) = span "c" x
    | token_to_html5 (Punct x) = span "p" x
    | token_to_html5 (Whitespace x) = x
    | token_to_html5 (Raw x) = escape_html x;
in
fun emit_html5 tokens =
  foldl (fn (tok,s) => s ^ (token_to_html5 tok))
        ""
        tokens
end;

end;

