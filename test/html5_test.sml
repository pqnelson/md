fun html5 (s : string) (header : string) (footer : string) =
    generic_header ^
    header ^
    "\n</head>\n<body>\n" ^
    "<main>\n" ^
    (Html5.emit id (Md.parse s)) ^
    "\n" ^
    footer ^
    "</main>\n" ^
    "\n</body>\n" ^
    "</html>\n";

fun html5_test name 
               (expected_lines : string list)
               (actual_lines : string list) =
    test name
         (fn () =>
             let
               val expected = concat expected_lines;
               val actual = html5 (String.concatWith
                                       "\n"
                                       actual_lines)
                                  ""
                                  "";
             in
               assert_eq (expected, actual,
                          ("## EXPECTED: \"" ^
                           expected ^
                           "\"\n" ^
                           "## ACTUAL: \"" ^
                           actual ^
                           "\"\n"))
             end);

val html5_test1 =
html5_test "simple_html5_test1"
           [generic_header,
            "\n</head>\n<body>\n<main>\n",
            "\n<h1> Introduction\n</h1>\n",
            "\n<p>\n",
            "These open the file named name for input and \n",
            "output, respectively. If name is a relative \n",
            "pathname, the file opened depends on the current\n",
            " working directory. On openOut, the file is \n",
            "created if it does not already exist and truncated\n",
            " to length zero otherwise. It raises Io if a \n",
            "stream cannot be opened on the given file, or in\n",
            " the case of openIn, the file name does not exist.",
            "\n</p>\n",
            "\n</main>\n\n</body>\n",
            "</html>\n"]
           ["# Introduction",
            "",
            "These open the file named name for input and ",
            "output, respectively. If name is a relative ",
            "pathname, the file opened depends on the current",
            " working directory. On openOut, the file is ",
            "created if it does not already exist and truncated",
            " to length zero otherwise. It raises Io if a ",
            "stream cannot be opened on the given file, or in",
            " the case of openIn, the file name does not exist."
           ];


register_suite "html5_test/" [
  html5_test1
];
