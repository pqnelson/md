structure LexerTest : SUITE = struct
fun assert_token_eq (expected, actual) =
    Assert.eq(expected,
              actual,
              "\n## Expected token '" ^
              (serialize_token expected) ^ "',\n" ^
              "## Actual token '" ^
              (serialize_token actual) ^ "'");

(*
The `expected_tokens` are *all* the expected tokens to be
produced until the lexer is finished.

This will even test if the lexer is finally finished after
trying to match against the expected tokens.
*)
fun mk_test name lines expected_tokens =
  Test.new name
           (fn () =>
               let
                 val s = (String.concatWith "\n" lines);
                 val lexer = Lexer.mk_lexer s;
               in
                 Assert.!!
                     (Lexer.is_finished (
                         foldl (fn (expected, l) : Token * Lexer.t =>
                           let
                             val (next, actual, _) = Lexer.lex l
                           in
                             (assert_token_eq(expected,actual);
                              next)
                           end)
                       lexer
                       expected_tokens))
                     "Lexer is finished"
             end);

val test1 = mk_test "test1"
                    ["This is a test",
                     "of a bunch of",
                     "",
                     "text separated",
                     "by newline"]
                    [TextToken "This is a test",
                    NewlineToken,
                    TextToken "of a bunch of",
                    NewlineToken,
                    NewlineToken,
                    TextToken "text separated",
                    NewlineToken,
                    TextToken "by newline"];

val test_header1 =
    mk_test "test_header1"
            ["# Header One",
             "",
             "This is a test",
             "of a bunch of",
             "",
             "text separated",
             "by newline"]
            [HashToken,
             TextToken " Header One",
             NewlineToken,
             NewlineToken,
             TextToken "This is a test",
             NewlineToken,
             TextToken "of a bunch of",
             NewlineToken,
             NewlineToken,
             TextToken "text separated",
             NewlineToken,
             TextToken "by newline"];

val test_header2 =
    mk_test "test_header2"
            ["# Header One",
             "",
             "This is a test",
             "of a bunch of",
             "",
             "## Subheader Two",
             "",
             "text separated",
             "by newline"]
            [HashToken,
             TextToken " Header One",
             NewlineToken,
             NewlineToken,
             TextToken "This is a test",
             NewlineToken,
             TextToken "of a bunch of",
             NewlineToken,
             NewlineToken,
             HashToken, HashToken, TextToken " Subheader Two",
             NewlineToken,
             NewlineToken,
             TextToken "text separated",
             NewlineToken,
             TextToken "by newline"];

val test_mistake_header1 =
    mk_test "test_mistake_header1"
            ["This is a #1 test",
             "of a bunch of",
             "",
             "text separated",
             "by newline"]
            [TextToken "This is a ",
             HashToken,
             TextToken "1 test",
             NewlineToken,
             TextToken "of a bunch of",
             NewlineToken,
             NewlineToken,
             TextToken "text separated",
             NewlineToken,
             TextToken "by newline"];

val test_mistaken_identity_tokens1 =
    mk_test "test_mistaken_identity_tokens1"
            ["This is a #1 test",
             "of a bunch of",
             "Cauchy inequality $a>(b-1)$",
             "[text separated]",
             "by newline"]
            [TextToken "This is a ",
             HashToken,
             TextToken "1 test",
             NewlineToken,
             TextToken "of a bunch of",
             NewlineToken,
             TextToken "Cauchy inequality $a",
             BlockquoteToken,
             LeftParenToken,
             TextToken "b-1",
             RightParenToken,
             TextToken "$",
             NewlineToken,
             LeftBrackToken,
             TextToken "text separated",
             RightBrackToken,
             NewlineToken,
             TextToken "by newline"];

val test_comment1 =
    mk_test "test_comment1"
            ["This is a test",
             "of a bunch of <!--comment this out-->",
             "",
             "text separated",
             "by newline"]
            [TextToken "This is a test",
             NewlineToken,
             TextToken "of a bunch of ",
             StartCommentToken,
             TextToken "comment this out",
             EndCommentToken,
             NewlineToken,
             NewlineToken,
             TextToken "text separated",
             NewlineToken,
             TextToken "by newline"];


val suite = Test.register_suite "lexer_test/" [
    test1
  , test_header1
  , test_header2
  , test_mistake_header1
  , test_mistaken_identity_tokens1
  , test_comment1
  ];

end;
