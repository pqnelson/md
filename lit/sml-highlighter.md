---
title: Standard ML Syntax Highlighter
---

# Standard ML Syntax Highlighter

The basic idea for the Standard ML syntax highlighter is to perform
the following steps:
1. Determine the next token: it could be an alphanumeric identifier, a
   symbolic identifier, whitespace, string, numeric,
   or a symbolic separator (like a parentheses, comma, bracket, etc.)
2. Determine the token class
3. Translate it to the appropriate class
4. If we're not done, go back to step 1

## Test if the character is a symbolic identifier

A symbolic identifier in Standard ML consists of one or more of the
following characters: `` !#$%&*+-/:<=>?@\\^`|~ ``

We can turn this into a trie, and test for membership. Although this
seems really complicated, it reduces the number of comparisons from 20
to 5 in the worst case scenario.

We simply tabulate all the code points from 0 to 127, then filter out
the ones which correspond to symbolic identifier characters, and test
the that the remaining characters are not symbolic identifiers. The
average number of comparison necessary when considering the uniform
distribution on the 20 symbolic characters is 3.1 comparisons.

(If we tried the same thing with a linear lookup, testing each
character of the string for equality, then the uniform distribution
gives us an average of 10.5 comparisons. Since sqrt(10.5) ~ 3.24 >
3.1, this is enough of an improvement to keep the difference.)

Actually, having written this code, it seems like we could _refactor_
it so we have a helper function _parameterized_ by the string of
possible characters in the token.

<details>
<summary>Tests for symbolic identifier predicates</summary>

We should test that every symbolic character is recognized. That is to
say, every char in `` !#$%&*+-/:<=>?@\^`|~ `` is considered a
"symbolic identifier char".

We can create a helper function which takes a character `c`, then
produces a unit test asserting it should be recognized as a symbolic
identifier character.

This will let us produce a test suite for the valid symbolic
identifier characters.

```sml {file=sml_highlighter_test.sml}
val every_symbolic_char_recognized_suite =
  let
    fun mk_test (c : char) =
      Test.new ((str c) "_is_symbolic_test")
               (fn () =>
                   Assert.!! (is_symbolic_identifier c)
                          ("EXPECTED: " ^
                           (str c) ^
                           " should be a symbolic identifier"));
  in
    Test.suite "every_symbolic_char_recognized_suite"
               (map mk_test
                    (explode "!#$%&*+-/:<=>?@\\^`|~"))
  end;
```

However, it is not enough to merely test symbolic identifier
characters are correctly recognized. We should also test that all
other characters are not mistaken to be a symbolic identifier
character. 

We will test all other ASCII characters are not mistaken to be
identifiers. 

```sml {file=sml_highlighter_test.sml}
structure List = struct
  open List;
  fun member x [] = false
    | member x (y :: ys) = (x = y) orelse (member x ys);
end;

val every_nonsymbolic_char_recognized_suite =
  let
    fun mk_test (c : char) =
      Test.new ((str c) "_is_not_symbolic_test")
               (fn () =>
                   Assert.!! not (is_symbolic_identifier c)
                          ("EXPECTED: " ^
                           (str c) ^
                           " [ord: " ^
                           (Int.toString (ord c)) ^
                           "] " ^
                           " should be not a symbolic identifier"));
    val symbolic_codepoints = map ord (explode "!#$%&*+-/:<=>?@\\^`|~");
    
    val all_other_cps = List.filter
                          (fn (n : int) => n > 0)
                          (List.tabulate(128,
                                        fn n =>
                                           if List.member n symbolic_codepoints
                                           then ~1
                                           else n));
  in
    Test.suite "every_nonsymbolic_char_recognized_suite"
               (map (mk_test o chr)
                    all_other_cps);
  end;
```

These two test suites ensure that among all 128 ASCII characters, only
the 20 characters which should be considered viable constituents for
symbolic identifiers _are correctly determined_.

Now, testing for a symbolic identifier substring. What should happen
if --- somehow --- it is accidentally applied to a string which **does not**
begin with a symbolic identifier character? We should return the empty
string and the start position.

We should test for the most common situations where the leading
character is not a symbolic identifier character.

```sml {file=sml_highlighter_test.sml}
local
  fun mk_test (test_name : string) (arg : string) =
    Test.new test_name
             (fn () =>
               let
                 val (actual as (s,start)) =
                   (symbolic_identifier arg 0);
               in
                 Assert.eq ("", 0)
                           actual
                           ("EXPECTED: (\"\", 0)\nACTUAL: (" ^
                            s ^
                            ", " ^
                            (Int.toString start) ^
                            ")")
               end);
in
val symbolic_identifier_applied_wrong_suite =
  Test.suite
    "symbolic_identifier_applied_wrong_suite"
    [ mk_test "symbolic_identifier_applied_wrong_test1"
              "accident"
    , mk_test "symbolic_identifier_applied_wrong_test2"
              " _accident"
    , mk_test "symbolic_identifier_applied_wrong_test3"
              ".accident"
    , mk_test "symbolic_identifier_applied_wrong_test4"
              "\naccident"
    , mk_test "symbolic_identifier_applied_wrong_test5"
              "\taccident"
    , mk_test "symbolic_identifier_applied_wrong_test6"
              "[accident"
    , mk_test "symbolic_identifier_applied_wrong_test7"
              "]accident"
    , mk_test "symbolic_identifier_applied_wrong_test8"
              ";accident"
    , mk_test "symbolic_identifier_applied_wrong_test9"
              ",accident"
    , mk_test "symbolic_identifier_applied_wrong_test10"
              "(accident"
    , mk_test "symbolic_identifier_applied_wrong_test11"
              ")accident"
    ]
end;
```

We should also provide a few tests for when we pass in a string which
begins properly with a symbolic identifier starting at the specific
location. 

```sml
local
  fun mk_test name s start (expected as (x,n)) =
    Test.new name
             (fn () =>
                 let
                   val (actual as (s',cur)) =
                     symbolic_identifier s start;
                   val msg = "EXPECTED: (" ^
                             x ^
                             ", " ^
                             (Int.toString n) ^
                             ")\nACTUAL: (" ^
                             s' ^
                             (Int.toString cur) ^
                             ")";
                 in
                   Assert.eq expected actual msg
                 end);
in
val symbolic_identifier_applied_correctly_suite =
  Test.suite
    "symbolic_identifier_applied_correctly_suite"
    [ mk_test "starts_with_symbolic_id_test1"
              ":+: foo bar"
              0
              (":+:", 3)
    , mk_test "has_symbolic_id_test1"
              "spam :+: foo bar"
              (size "spam ")
              (":+:", size "spam: " + size ":+:")
    ]
end;
```
</details>

```sml {file=sml_highlighter.sml}
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
    (*
  val symbolic_chars_str = "!#$%&*+-/:<=>?@\\^`|~";
  val symbolic_codepoints = (IntVector.fromList o
                             (map ord) o
                             explode)
                              symbolic_chars_str;
  *)
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
```

## Alphanumeric identifiers

The other cast of identifiers are alphanumeric identifiers (including
"reserved" keywords).

These start with either an apostrophe and then a letter (`'foo`) or a letter
(`spam_machine_2000`), and then include any alphanumeric letter,
underscore `_`, or apostrophe `'`. We will be a bit more generous and
consider `'` followed by anything to be a alphanumeric identifier.

So we will **intentionally** deviate and allow `'32` to be considered
an "alphanumeric identifier".

<details>
<summary>Specification and Tests for alphanumeric identifier tokenization</summary>

```sml {file=sml_highlighter_test.sml}
local
  fun mk_test name lexeme start (expected as (s,n)) =
    Test.new
      name
      (fn () =>
          let
            val expected = ("'32", 3);
            val (actual as (s',n')) = alphanum_identifier
                                        lexeme
                                        start;
          in
            Assert.eq expected
                      actual
                      ("EXPECTED: (" ^
                       s ^
                       ", " ^
                       (Int.toString n) ^
                       ")\n" ^
                       "ACTUAL: (" ^
                       s' ^
                       ", " ^
                       (Int.toString n') ^
                       ")\n")
          end);
in
Test.suite
  "alphanumeric_identifier_suite"
  [ mk_test "apostrophe_int_identifier_test1"
            "'32"
            0
            ("'32", 3)
  , mk_test "typevar_identifier_test1"
            "'a list"
            0
            ("'a", 2)
  , mk_test "struct_name_identifier_test1"
            "structure SpamMachine = struct"
            (size "structure ")
            ("SpamMachine", size "structure SpamMachine")
  , mk_test "int_is_not_alphanum_test1"
            "31 is awesome"
            0
            ("", 0)
  , mk_test "colon_is_not_alphanum_test1"
            ":foo is awesome"
            0
            ("", 0)
  , mk_test "semicolon_is_not_alphanum_test1"
            "; foo is awesome"
            0
            ("", 0)
  , mk_test "leading_underscore_is_not_alphanum_test1"
            "_foo is awesome"
            0
            ("", 0)
  , mk_test "leading_int_is_not_alphanum_test1"
            "3foo is awesome"
            0
            ("", 0)
  ]
end;
```

</details>

```sml {file=sml_highlighter.sml}
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
```

## Numeric Tokens

A number can look like "~?\d+" or "~?0x[0-9a-fA-F]+" or "0w\d+" or "0wx[0-9a-fA-F]+".

We just need two helper functions for `\d+` and `[0-9a-fA-F]+`, as
well as helper functions for testing if we start one sort of number or
another.

**Caveat:** We are intentionally ignoring real numbers, because we do
not intend to use them anywhere in our endeavour.

```sml {file=sml_highlighter.sml}
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
```

## String Tokens

We need to take care to handle escaped quotation marks as the end of a
string. 

```sml {file=sml_highlighter.sml}
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
```

## Comment Tokens

We should read through the comments and treat them as a single token.

```sml {file=sml_highlighter.sml}
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
```

## Miscellaneous Tokens

These are the strange tokens which cannot form an identifier, string,
or number --- like parentheses or commas.

These are specifically: '(', ')', '[', ']', '{', '}', ',', ';', '...', '_'.

```sml
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
```

## White Space

We need to skip white space.

```sml
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
```

## Tokenizer

We use a trie for determining if an identifier is a reserved
keyword. Again, when using a uniform distribution over all the
reserved keywords, this changes the average number of comparisons from
21 (if we just looked through the linked list for a match) to 4 (using
a trie). This is a considerable improvement, especially since the
height of the trie is 6. (Since most identifiers will not be reserved,
this improves performance overall by a factor of 7.)

``` sml
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
```

Now, we can use this in our giant `tokenize` function to determine if
the identifier is a reserved keyword or not.

We eventually will parametrize this function with a callback to
replace an identifier with a link.

Well, we should really be tracking more state than just this. For
example, we should have this make link anchors for defining new
identifiers.

```sml
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
```

We also want an `html5 : Token list -> string` function to generate
the HTML5 for the stuff. Note that we perform string escaping here.

``` sml
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
```

...and that's all I think we need?

```sml
end;
```

