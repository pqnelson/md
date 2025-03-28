fun serialize_block (block : string Block) =
    Block.serialize id block;

structure MdTest : SUITE = struct

fun assert_ast_eq (expected : string Block list)
                  (lines : string list) =
    (fn () =>
        let
          val (_, actual) = Md.parse (String.concatWith "\n" lines);
          val msg = "\n## EXPECTED: [" ^
                    (String.concatWith
                         ", "
                         (map serialize_block expected)) ^
                    "]\n" ^
                    "## ACTUAL:   [" ^
                    (String.concatWith
                         ", "
                         (map serialize_block actual)) ^
                    "]\n"
        in
          Assert.!! (expected = actual) msg
        end);

fun ast_test name expected actual =
    Test.new name (assert_ast_eq expected actual);

val header_test1 =
    ast_test "header_test1"
             [Heading (2, [Text " This is an example"]),
              Quote [Par [Text "Blockquote 1"]]]
             ["## This is an example",
              "",
              "> Blockquote 1"];

(* That's 10 "#" leading the header *)
val header_test2 =
    ast_test "header_test2"
             [Heading (6, [Text " This is another example"]),
              Par [Text "Blah Blah Blah"]]
             ["########## This is another example",
              "",
              "Blah Blah Blah"];

val header_test3 =
    ast_test "header_test3"
             [Heading (1, [Text " This is another example ",
                           Anchor "anchor-for-header"]),
              Par [Text "Blah Blah Blah"]]
             ["# This is another example [#anchor-for-header]",
              "",
              "Blah Blah Blah"];

val bq_test1 =
    ast_test "blockquote_test1"
             [Quote [Quote [Par [Text "This is another example"]],
                     Par [Text "Blah Blah Blah"]]
             ]
             [">> This is another example",
              "> ",
              "> Blah Blah Blah"];
                                  
val bq_test2 =
    ast_test "blockquote_test2"
             [Quote [Quote [Par [Text "This is another example"]]],
              Quote [Par [Text "Blah Blah Blah"]]
             ]
             [">> This is another example",
              "",
              "> Blah Blah Blah"];

val bq_test3 =
    ast_test "blockquote_test3"
             [Quote [Par [Text "This is another example"],
                     Pre {code = "Blah Blah Blah",
                          language = NONE,
                          is_example = false}]
             ]
             ["> This is another example",
              "> ",
              "> ```",
              "> Blah Blah Blah",
              "> ```"];

val bq_test4 =
    ast_test "blockquote_test4"
             [Quote [Par [Text "This is another example"],
                     Pre { code = "Blah Blah Blah"
                         , language = NONE
                         , is_example = false}]
             ]
             ["> This is another example",
              "> ```",
              "> Blah Blah Blah",
              "> ```"];

val pre_test1 =
    ast_test "pre_test1 (code block with language name given)"
             [Pre { code = "Blah Blah Blah"
                  , language = SOME "brainfuck"
                  , is_example = false
                  }]
             ["```brainfuck",
              "Blah Blah Blah",
              "```"];

val pre_test2 =
    ast_test "pre_test2 (example nested code block)"
             [Pre { code = "    ```\n    example code block\n    ```"
                  , language = NONE
                  , is_example = false
                  }]
             ["```",
              "    ```",
              "    example code block",
              "    ```",
              "```"];

val pre_test3 =
    ast_test "pre_test3 (example code block)"
             [Pre { code = "    ```\n    example code block\n    ```"
                  , language = SOME "sml"
                  , is_example = true
                  }]
             ["```sml*",
              "    ```",
              "    example code block",
              "    ```",
              "```"];

val pre_test4 =
    ast_test "pre_test4 (example code block)"
             [Pre { code = "    ```\n    example code block\n    ```"
                  , language = SOME "sml"
                  , is_example = true
                  }]
             ["```sml {example}",
              "    ```",
              "    example code block",
              "    ```",
              "```"];

val pre_test5 =
    ast_test "pre_test5 (example code block)"
             [Pre { code = "    ```\n    example code block\n    ```"
                  , language = SOME "sml"
                  , is_example = false
                  }]
             ["```sml {example=false}",
              "    ```",
              "    example code block",
              "    ```",
              "```"];

val pre_test6 =
    ast_test "pre_test6 (example code block)"
             [Pre { code = "    ```\n    example code block\n    ```"
                  , language = SOME "sml"
                  , is_example = true
                  }]
             ["```sml {example=spam}",
              "    ```",
              "    example code block",
              "    ```",
              "```"];

val pre_test7 =
    ast_test "pre_test7 (example code block)"
             [Pre { code = "    ```\n    example code block\n    ```"
                  , language = SOME "sml"
                  , is_example = true
                  }]
             ["```sml {example=FaLsE}",
              "    ```",
              "    example code block",
              "    ```",
              "```"];

(* Github does this, as of 19 August 2024 *)
val mixed_inline_test1 =
    ast_test "mixed_inline_test1"
             [Par [Text "xxx ",
                   Bold [Text "yyy _zzz aaa"],
                   Text " bbb_ ccc"]]
             ["xxx **yyy _zzz aaa** bbb_ ccc"];

(* Github does this, as of 19 August 2024 *)
val mixed_inline_test2 =
    ast_test "inline_test2"
             [Par [Text "xxx ",
                   Emph [Text "yyy **zzz aaa"],
                   Text " bbb** ccc"]]
             ["xxx _yyy **zzz aaa_ bbb** ccc"]

val inline_code_test1 =
    ast_test "inline_code_test1"
             [Par [Text "xxx ",
                   Code "yyy",
                   Text " zzz"]]
             ["xxx `yyy` zzz"];

val inline_code_test2 =
    ast_test "inline_test2"
             [Par [Text "xxx ",
                   Code "yyy` zzz",
                   Text " www"]]
             ["xxx ``yyy` zzz`` www"];

val inline_code_test3 =
    ast_test "inline_test3"
             [Par [Text "XXX ",
                   Code "YYY``` ZZZ",
                   Text " WWW"]]
             ["XXX ``YYY``` ZZZ`` WWW"];

val inline_code_test4 =
    ast_test "inline_test4"
             [Par [Text "If I want to include 'named code chunks' as WEB,\nCWEB, and NOWEB all support --- with ",
                   Code "<<code chunk name>>=",
                   Text " as the first\nline following the ",
                   Code "```language",
                   Text " declaration --- then I need to make\nsure I replace all instances of ",
                   Code "<<code chunk name>>",
                   Text " with its contents\nwhen ",
                   Code "tangle",
                   Text " is invoked, and have ",
                   Code "weave",
                   Text " link back to the code block\ndefining this code chunk."
             ]]
             ["If I want to include 'named code chunks' as WEB,"
, "CWEB, and NOWEB all support --- with `<<code chunk name>>=` as the first"
, "line following the `` ```language `` declaration --- then I need to make"
, "sure I replace all instances of `<<code chunk name>>` with its contents"
, "when `tangle` is invoked, and have `weave` link back to the code block"
, "defining this code chunk."];

val inline_code_test5 =
  ast_test "inline_code_test5"
             [Par [Text "If I want to include 'named code chunks' as WEB,\nCWEB, and NOWEB all support --- with `<<code chunk name>>=` as the first\nline following the ",
                   Code "```language",
                   Text " declaration --- then I need to make\nsure I replace all instances of ",
                   Code "<<code chunk name>>",
                   Text " with its contents\nwhen ",
                   Code "tangle",
                   Text " is invoked, and have ",
                   Code "weave",
                   Text " link back to the code block\ndefining this code chunk."
             ]]
             ["If I want to include 'named code chunks' as WEB,"
, "CWEB, and NOWEB all support --- with \\`<<code chunk name>>=\\` as the first"
, "line following the `` ```language `` declaration --- then I need to make"
, "sure I replace all instances of `<<code chunk name>>` with its contents"
, "when `tangle` is invoked, and have `weave` link back to the code block"
, "defining this code chunk."];

val url_test1 =
    ast_test "url_test1"
             [Par [Text "xxx ",
                   Link { link_url = "http://url.com"
                        , link_desc = [Text "yyy"]},
                   Text " zzz"]]
             ["xxx [yyy](http://url.com) zzz"];

val url_test2 =
    ast_test "url_test2"
             [Par [Text (String.concatWith "\n" ["Paul E. Black, 'dictionary', ",
                   "in Dictionary of Algorithms and Data Structures (online),",
                   "Paul E. Black, ed. 2 November 2020. (accessed 27 November 2024)", 
                   "Available from: "]),
                   Link { link_url = "https://www.nist.gov/dads/HTML/dictionary.html"
                        , link_desc = [Text "https://www.nist.gov/dads/HTML/dictionary.html"]}
                   ]]
             ["Paul E. Black, 'dictionary', ",
  "in Dictionary of Algorithms and Data Structures (online),",
  "Paul E. Black, ed. 2 November 2020. (accessed 27 November 2024)", 
  "Available from: [https://www.nist.gov/dads/HTML/dictionary.html](https://www.nist.gov/dads/HTML/dictionary.html)"];

(* bug which actually happened :( *)
val url_test3 =
    ast_test "url_test3"
             [Par [Text (String.concatWith "\n" ["Paul E. Black, 'dictionary', ",
                   "in Dictionary of Algorithms and Data Structures [online],",
                   "Paul E. Black, ed. 2 November 2020. (accessed 27 November 2024)", 
                   "Available from: "]),
                   Link { link_url = "https://www.nist.gov/dads/HTML/dictionary.html"
                        , link_desc = [Text "https://www.nist.gov/dads/HTML/dictionary.html"]}
                   ]]
             ["Paul E. Black, 'dictionary', ",
  "in Dictionary of Algorithms and Data Structures [online],",
  "Paul E. Black, ed. 2 November 2020. (accessed 27 November 2024)", 
  "Available from: [https://www.nist.gov/dads/HTML/dictionary.html](https://www.nist.gov/dads/HTML/dictionary.html)"];

val url_test4 =
    ast_test "url_test4"
             [Par [Text "xxx ",
                   Link { link_url = "https://en.wikipedia.org/wiki/Work_(thermodynamics)"
                        , link_desc = [Text "yyy"]},
                   Text " zzz"]]
             ["xxx [yyy](https://en.wikipedia.org/wiki/Work_(thermodynamics)) zzz"];

val anchor_test1 =
    ast_test "anchor_test1"
             [Par [Text "xxx ",
                   Anchor "definition-1",
                   Text " yyy"]]
             ["xxx [#definition-1] yyy"];

val img_test1 =
    ast_test "img_test1"
             [Par [Text "xxx ",
                   Image { img_alt="The Mona Lisa"
                         , img_src="https://en.wikipedia.org/wiki/Mona_Lisa#/media/File:Mona_Lisa,_by_Leonardo_da_Vinci,_from_C2RMF_retouched.jpg"},
                   Text " yyy"]]
             ["xxx ![The Mona Lisa](https://en.wikipedia.org/wiki/Mona_Lisa#/media/File:Mona_Lisa,_by_Leonardo_da_Vinci,_from_C2RMF_retouched.jpg) yyy"];

val link_test1 =
    ast_test "link_test1"
             [Par [Link {link_desc=[Text "Isabelle"]
                                ,link_url="https://isabelle.in.tum.de/library/Doc/Implementation/ML.html"}]]
             ["[Isabelle](https://isabelle.in.tum.de/library/Doc/Implementation/ML.html)"];

val link_test2 =
    ast_test "link_test2"
             [UList [[Par [Link {link_desc=[Text "Isabelle"]
                                ,link_url="https://isabelle.in.tum.de/library/Doc/Implementation/ML.html"}]]]]
             ["- [Isabelle](https://isabelle.in.tum.de/library/Doc/Implementation/ML.html)"];

val TeX_test1 =
  ast_test "TeX_test1"
           [Par [ Text "Einstein did not prove $E=mc^{2}$ but "
                , Emph [Text "instead proved $E^{2}c^{2}=\\eta^{\\mu\\nu}p_{\\mu}p_{\\nu}$ which is far more useful!"]
           ]]
           ["Einstein did not prove $E=mc^{2}$ but _instead proved $E^{2}c^{2}=\\eta^{\\mu\\nu}p_{\\mu}p_{\\nu}$ which is far more useful!_"];

val TeX_test2 =
  ast_test "TeX_test2"
           [Par [ Text "Einstein did not prove $E=mc^{2}$ but _instead proved $E^{2}c^{2}=\\eta^{\\mu\\nu}p_{\\mu}p_{\\nu}$ which is far more useful!"]]
           ["Einstein did not prove $E=mc^{2}$ but _instead proved $E^{2}c^{2}=\\eta^{\\mu\\nu}p_{\\mu}p_{\\nu}$ which is far more useful!"];

val TeX_test3 =
  ast_test "TeX_test3"
           [Par [ Text "I "
                , Bold [Text "love"]
                , Text " convolutions! So much so that I "
                , Bold [Text "hacked TeX to write \\begin{equation}f**g=\\int(mess)dx\\end{equation} when encountering two asterisks!"]]]
           [concat [
               "I **love** convolutions! "
             , "So much so that I **hacked TeX to write "
             , "\\begin{equation}f**g=\\int(mess)dx\\end{equation}"
             , " when encountering two asterisks!**"]];

val ul_test6 =
  ast_test "ul_test6"
           [UList [[Par [Text "We want predicates testing if the test result records a success,\nfailure, or error."]]
                  ,[Par [Text "We want to count the number of successes, failures, and errors which\noccurred when running a test. For test cases, these will be at most\n1."]]]]
           ["- We want predicates testing if the test result records a success,"
           ,"  failure, or error."
           ,"- We want to count the number of successes, failures, and errors which"
           ,"  occurred when running a test. For test cases, these will be at most"
           ,"  1."];

val ul_test7 =
  ast_test "ul_test7"
           [UList [[Par [Text "We want predicates testing if the test result records a success,\nfailure, or error."]]
                  ,[Par [Text "We want to count the number of successes, failures, and errors which\noccurred when running a test. For test cases, these will be at most"]]],
            Par [Text "This should be a paragraph."]]
           ["- We want predicates testing if the test result records a success,"
           ,"  failure, or error."
           ,"- We want to count the number of successes, failures, and errors which"
           ,"  occurred when running a test. For test cases, these will be at most"
           ,"  "
           ,"This should be a paragraph."];


val ul_test8 =
  ast_test "ul_test8"
           [UList [[Par [Text "This is a list\nof various items"]]
                  ,[Par [Text "Otherwise, we have to do:"]
                   ,OList [[Par [Text "More work.\nThat sucks."]]
                          ,[Par [Text "Or else we have the following\ncases."]
                           ,OList [[Par [Text "We think hard\nand we get paid for it"]]
                                  ,[Par [Text "Then we write\nand write"]]]]]]]]
           ["- This is a list",
            "  of various items",
            "- Otherwise, we have to do:",
            "  1. More work.",
            "     That sucks.",
            "  2. Or else we have the following",
            "     cases.",
            "     1. We think hard",
            "        and we get paid for it",
            "     2. Then we write",
            "        and write"
           ];

val ul_test9 =
  ast_test "ul_test9"
[Par [Text "The flow of logic is remarkably similar to the situation where we\nextracting an \"entry\" in the key-value pairs:"]
,UList [[Par [Text "If the position for the next key-value pair is greater than the\nlength of the substring (representing the code block), then we raise\na runaway exception."]], [Par [Text "If the position for the next key-value pair is positive and the\ncharacter located there is equal to the close brace, then we're\ndone. We should return the accumulated list of key-value pairs and\nthe position of the close brace (or the length of the substring,\nwhichever is smaller)."]], [Par [Text "Otherwise, we get to the interesting part: there's work to be done. "], OList [[Par [Text "Extract the key using the ", Code "extract_entry", Text " invoked at the given\nposition. This will give us the key as a substring ", Emph [Text "and"], Text " the\nposition in the substring immediately after the key. We trim the\nwhitespace from the key, and turn it into a proper string."]], [Par [Text "Now we try to find the value. This itself decomposes into several\ncases."], OList [[Par [Text "We have not exhausted the substring describing the code block,\nand we have run into a comma. This happens with ", Code "{foo, key = value,...}", Text "\nand we decided to treat this as ", Code "{foo = foo, ...}", Text ".\nThen we iterate, calling ", Code "extract_iter", Text " again but with the\nposition of the next character updated and pushing ", Code "(k,k)", Text " on\nthe accumulator"]], [Par [Text "We have not exhausted the substring and we have run into the\nclosing brace ", Code "}", Text " for the metadata. Then we just do as before,\ntreat the value as equal to the key, but we terminate the\nfunction returning ", Code "(k,k)::acc", Text " and the location where the\nclosing brace occurred."]]]]]]]]
 ["The flow of logic is remarkably similar to the situation where we",
"extracting an \"entry\" in the key-value pairs:",
"",
"- If the position for the next key-value pair is greater than the",
"  length of the substring (representing the code block), then we raise",
"  a runaway exception.",
"- If the position for the next key-value pair is positive and the",
"  character located there is equal to the close brace, then we're",
"  done. We should return the accumulated list of key-value pairs and",
"  the position of the close brace (or the length of the substring,",
"  whichever is smaller).",
"- Otherwise, we get to the interesting part: there's work to be done. ",
"  1. Extract the key using the `extract_entry` invoked at the given",
"     position. This will give us the key as a substring _and_ the",
"     position in the substring immediately after the key. We trim the",
"     whitespace from the key, and turn it into a proper string.",
"  2. Now we try to find the value. This itself decomposes into several",
"     cases.",
"     1. We have not exhausted the substring describing the code block,",
"        and we have run into a comma. This happens with `{foo, key = value,...}`",
"        and we decided to treat this as `{foo = foo, ...}`.",
"        Then we iterate, calling `extract_iter` again but with the",
"        position of the next character updated and pushing `(k,k)` on",
"        the accumulator",
"     2. We have not exhausted the substring and we have run into the",
"        closing brace `}` for the metadata. Then we just do as before,",
"        treat the value as equal to the key, but we terminate the",
"        function returning `(k,k)::acc` and the location where the",
"        closing brace occurred.",
""
];

val suite = Test.register_suite "md_test/" [
    Test.suite "block/" [
      header_test1
, header_test2
, header_test3
, link_test1
, link_test2
, bq_test1
, bq_test2
, bq_test3
, bq_test4
, pre_test1
, pre_test2
, pre_test3
, pre_test4
, pre_test5
, pre_test6
, pre_test7
, (ast_test "ul_test1"
            [UList [[Par [Text "This is an item"]],
                    [Par [Text "This is the second item"]]]]
            ["- This is an item",
             "- This is the second item"])
, (ast_test "ul_test2"
            [UList [[Par [Text "This is an item"]],
                    [Par [Text "This is the second item"]]]]
            ["+ This is an item",
             "+ This is the second item"])
, (ast_test "ul_test3"
            [UList [[Par [Text "This is an item"],
                     Par [Text "And a second paragraph in the first item."]],
                    [Par [Text "This is the second item"]]],
            Par [Text "Just some text"]]
            ["- This is an item",
             "  ",
             "  And a second paragraph in the first item.",
             "- This is the second item",
             "",
             "Just some text"])
, (ast_test
   "ul_test4"
   [UList [[Par [Text "This is an item"],
            UList [[Par [Text "A sublist"]],
                   [Par [Text "And a second paragraph in the first item."]]]],
           [Par [Text "This is the second item"]]],
    Par [Text "Just some text"]]
   ["- This is an item",
    "  ",
    "  - A sublist",
    "  - And a second paragraph in the first item.",
    "- This is the second item",
    "",
    "Just some text"])
, (ast_test
   "ul_test5"
   [UList [[Par [Text "This is an item"],
            UList [[Par [Text "A sublist"]],
                   [Par [Text "And a second paragraph in the first item."]]]],
           [Par [Text "This is the second item"]]],
    Par [Text "Just some text"]]
   ["- This is an item",
    "  ",
    "  + A sublist",
    "  + And a second paragraph in the first item.",
    "- This is the second item",
    "",
    "Just some text"])
, ul_test6
, ul_test7
, ul_test8
, ul_test9
, (ast_test "ol_test1"
            [OList [[Par [Text "This is an item"]],
                    [Par [Text "This is the second item"]]]]
            ["1. This is an item",
             "2. This is the second item"])
, (ast_test "ol_test2 (irrelevance of numbering)"
            [OList [[Par [Text "This is an item"]],
                    [Par [Text "This is the second item"]]]]
            ["11. This is an item",
             "2. This is the second item"])
, TeX_test1
, TeX_test2
, TeX_test3
    ]
, Test.suite "inline/" [
    mixed_inline_test1
  , mixed_inline_test2
  , inline_code_test1
  , inline_code_test2
  , inline_code_test3
  , inline_code_test4
  , inline_code_test5
  , url_test1
  , url_test2
  , url_test3
  , url_test4
  , anchor_test1
  , img_test1
  ]
];
end;
