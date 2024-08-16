structure Lexer : LEXER = struct
type t = { s : string    (* input stream *)
         , line : int    (* line number of current position *)
         , pos : int     (* current position in s *)
         , len : int     (* length of the input stream s *)
         , offset : int  (* position in a line *)
         };

fun mk_lexer s =
    { s = s
    , line = 1
    , pos = 0
    , len = String.size s
    , offset = 1
    };

fun is_finished ({pos, len, ...} : t) =
    pos >= len;

(* These routines are needed specifically for adjusting the
   newline and offset, which could be included in position
   info for debugging purposes.
*)
fun is_newline ({s, len, pos, ...} : t) =
    String.sub(s, pos) = #"\n";

fun lex_newline ({s, len, pos, line, offset} : t) =
    ({ s = s
     , len = len
     , pos = pos + 1
     , line = line + 1
     , offset = 1
     }
    , NewlineToken
    , { line = line
      , start = offset
      }
    );

(* Blockquote must appear AFTER EndCommentToken,
because testing will be in this particular order.
 *)
val char_match = [ ("\n", NewlineToken, 1)
                 , ("_", UnderscoreToken, 1)
                 , ("*", AstToken, 1)
                 , ("(", LeftParenToken, 1)
                 , (")", RightParenToken, 1)
                 , ("[", LeftBrackToken, 1)
                 , ("]", RightBrackToken, 1)
                 , ("`", BacktickToken, 1)
                 , ("<!--", StartCommentToken, 4)
                 , ("-->", EndCommentToken, 3)
                 , ("#", HashToken, 1)
                 , (">", BlockquoteToken, 1)
                 ];
val non_ws_match = tl char_match;

(* This turns out to be 50% faster than string equality testing

- ASSUME: pos + lexeme_len < String.size(s)
- ASSUME: 0 <= i
- ASSUME: lexeme_len = String.size(lexeme)
 *)
fun lexeme_match s pos lexeme lexeme_len i =
    (i = lexeme_len) orelse
    ((String.sub(s, pos+i) = String.sub(lexeme, i)) andalso
     lexeme_match s pos lexeme lexeme_len (i + 1));

fun is_reserved ({s,len,...} : t) current (lexeme,_,tok_len) =
    (current + tok_len < len) andalso
    (lexeme_match s current lexeme tok_len 0);

fun is_char (lexer as {s, len,...} : t) current =
    (current < len) andalso (0 <= current) andalso
    (List.all (not o (is_reserved lexer current)) char_match);

fun lex_line (lexer as {s,len,pos,line,offset} : t) =
    let
      fun find_end (l : t) current =
          (if is_char l current
           then find_end l (current + 1)
           else current);
      val end_pos = find_end lexer pos;
    in
      ({ s = s
       , len = len
       , line = line
       , offset = offset + (end_pos - pos)
       , pos = end_pos
       }
      , TextToken (String.substring(s, pos, end_pos - pos))
      , { line = line
        , start = offset
        }
      )
    end;

(* Advance along the same line `delta` positions forward. *)
fun advance_lexer ({s,len,line,offset,pos} : t) delta =
    { s = s
    , len = len
    , line = line
    , offset = offset + delta
    , pos = pos + delta
    };

fun lex_token (lexer as {line,offset,...} : t) token token_len =
    (advance_lexer lexer token_len,
     token,
     { line = line
     , start = offset
     }
    );

fun matches_token ({s,pos,len,...} : t) (lexeme, token, tok_len)
    = (pos + tok_len < len) andalso
      (lexeme_match s pos lexeme tok_len 0);

fun lex (lexer as {line,offset,...} : t) =
  if is_finished lexer
  then (lexer, EofToken, {line = line, start=offset})
  else if is_newline lexer
  then lex_newline lexer
  else (case List.find (matches_token lexer) non_ws_match of
           (SOME (_, token, token_len)) => lex_token lexer
                                                     token
                                                     token_len
         |  NONE => lex_line lexer);
end;
