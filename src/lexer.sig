datatype Token = UnderscoreToken
               | AstToken
               | LeftParenToken
               | RightParenToken
               | LeftBrackToken
               | RightBrackToken
               | BacktickToken
               | StartCommentToken
               | EndCommentToken
               | HashToken
               | BlockquoteToken
               | TextToken of string
               | NewlineToken
       | EofToken;

val serialize_token =
 fn UnderscoreToken => "UnderscoreToken"
  | AstToken => "AstToken"
  | LeftParenToken => "LeftParenToken"
  | RightParenToken => "RightParenToken"
  | LeftBrackToken => "LeftBracketToken"
  | RightBrackToken => "RightBracketToken"
  | BacktickToken => "BacktickToken"
  | StartCommentToken => "StartCommentToken"
  | EndCommentToken => "EndCommentToken"
  | HashToken => "HashToken"
  | BlockquoteToken => "BlockquoteToken"
  | TextToken x => "TextToken(\""^x^"\")"
  | NewlineToken => "NewlineToken"
  | EofToken => "EofToken";

type Info = { line : int
            , start : int
            };

signature LEXER = sig
  type t;
  val mk_lexer : string -> t;
  val is_finished : t -> bool;
  val lex : t -> t * Token * Info;
end;
