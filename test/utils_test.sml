structure UtilsTest : SUITE = struct
fun mk_string_fn_test f name expected raw =
  Test.new name
       (fn () =>
           let
             val actual = f raw;
             val msg = ("## EXPECTED: '" ^
                        expected ^
                        "'\n## ACTUAL: '"^
                        actual ^ "'");
           in
           Assert.eq(expected, actual, msg)
           end);

fun mk_string_trim_test name expected raw =
    mk_string_fn_test string_trim name expected raw;

fun mk_string_to_lower_test name expected raw =
    mk_string_fn_test string_to_lower name expected raw;

fun mk_string_to_upper_test name expected raw =
    mk_string_fn_test string_to_upper name expected raw;

fun serialize_opt_i ((SOME s) : int option) = "SOME(\""^
                                              (Int.toString s)^
                                              "\")"
  | serialize_opt_i NONE = "NONE";

val string_indexof_from_test1 =
  Test.new "string_indexof_from_test1"
           (fn () =>
             let
               val s = "YYY``` ZZZ`` WWW";
               val actual = string_indexof_from "``" s 6;
               val expected = SOME 10;
               val msg = "## EXPECTED "^
                         (serialize_opt_i expected)^
                         "\n## ACTUAL "^
                         (serialize_opt_i actual)^"\n";
             in
               Assert.eq (expected, actual, msg)
             end);

val string_indexof_test1 =
  Test.new "string_indexof_test1"
           (fn () =>
             let
               val s = "YYY``` ZZZ`` WWW)";
               val actual = string_indexof ")" s;
               val expected = SOME 16;
               val msg = "## EXPECTED "^
                         (serialize_opt_i expected)^
                         "\n## ACTUAL "^
                         (serialize_opt_i actual)^"\n";
             in
               Assert.eq (expected, actual, msg)
             end);

val string_indexof_test2 =
  Test.new "str_indexof_test2"
         (fn () =>
             let
               val s = "- foo";
               val i = (case str_indexof (fn c => #" " = c) s of
                            SOME j => j
                          | NONE => ~1);
             in
               Assert.eq((SOME 1),
                         (str_indexof (fn c => #" " = c) s),
                         concat(["Index of ' ' in \"- foo\" ",
                                 "expected to be 1 ",
                                 "actual: ",
                                 Int.toString(i)]))
             end);

val suite = Test.register_suite "utils_test/" [
  mk_string_trim_test "string_trim_test1" "foo" "  foo"
, mk_string_trim_test "string_trim_test2" "bar and eggs" "bar and eggs   "
, mk_string_trim_test "string_trim_test3" "spam eggs" "  spam eggs   "
, mk_string_trim_test "string_trim_test4" "unchanged string" "unchanged string"
, mk_string_trim_test "string_trim_test5" "" ""
, mk_string_to_lower_test "string_to_lower_test1" "" ""
, mk_string_to_lower_test "string_to_lower_test2" "foo bar" "foo bar"
, mk_string_to_lower_test "string_to_lower_test3" "foo bar" "FoO bAr"
, mk_string_to_lower_test "string_to_lower_test4" "foo bar33" "FoO bAr33"
, mk_string_to_upper_test "string_to_upper_test1" "" ""
, mk_string_to_upper_test "string_to_upper_test2" "FOO BAR" "foo bar"
, mk_string_to_upper_test "string_to_upper_test3" "FOO BAR" "FoO bAr"
, mk_string_to_upper_test "string_to_upper_test4" "FOO BAR33" "FoO bAr33"
, string_indexof_from_test1
, string_indexof_test1
, string_indexof_test2
];

end;
