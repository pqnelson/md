
fun mk_string_fn_test f name expected raw =
  test name
       (fn () =>
           let
             val actual = f raw;
             val msg = ("## EXPECTED: '" ^
                        expected ^
                        "'\n## ACTUAL: '"^
                        actual ^ "'");
           in
           assert_eq(expected, actual, msg)
           end);

fun mk_string_trim_test name expected raw =
    mk_string_fn_test string_trim name expected raw;

fun mk_string_to_lower_test name expected raw =
    mk_string_fn_test string_to_lower name expected raw;

fun mk_string_to_upper_test name expected raw =
    mk_string_fn_test string_to_upper name expected raw;


register_suite "utils_test/" [
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
];
