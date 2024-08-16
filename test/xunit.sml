datatype TestOutcome = TestSuccess
                     | TestFail of string
                     | TestException of exn;

datatype Test = TestCase of string*(unit -> unit)
              | TestSuite of string*(Test list);

(* helper functions to make *writing* tests easier *)
val all_tests : (Test list) ref = ref [];

fun suite name tests = TestSuite(name,tests);
fun test name (f : unit -> unit) = TestCase(name, f);
fun register_suite name tests =
    let
      val s = suite name tests
    in
      all_tests := s::(!all_tests);
      s
    end;

(* basic assertion *)
exception AssertionFailure of string;

fun assert (msg : string) (is_success : bool) : unit =
    if is_success then () else raise AssertionFailure msg;

fun assert_eq (expected, actual, msg) =
    assert msg (expected = actual);

datatype TestResult = ResultCase of string * Time.time * TestOutcome
                    | ResultSuite of string * Time.time * (TestResult list);

fun run_test (test : Test) : TestResult =
let
  val start = Time.now()
in
  case test of
      (TestCase (case_name, assertion)) =>
      ((assertion();
        ResultCase (case_name, (Time.-)(Time.now(), start), TestSuccess))
       handle AssertionFailure msg => ResultCase (case_name,
                                                  (Time.-)(Time.now(), start),
                                                  TestFail msg)
            | e => ResultCase (case_name,
                               (Time.-)(Time.now(), start),
                               TestException e)
      )
    | (TestSuite (suite_name, tests)) =>
      (let
          val results = map run_test tests
          val dt = (Time.-)(Time.now(), start)
      in
          ResultSuite (suite_name, dt, results)
      end)
end;

(*
fun run_test_iter (results as {success,fail,error} : TestStats) path =
    fn (TestCase (name,f)) => (
      (case f () of
          TestSuccess => {success=success + 1, fail=fail, error=error}
        | TestFail msg => (print ("### Failure in test case " ^
                                  (path_name path name) ^ "\n");
                           print (msg ^ "\n");
                           {success=success, fail=fail + 1, error=error})
      ) handle e => (print ("### Error in test case " ^
                            (path_name path name) ^ "\n");
                     print ((exnName e) ^ ": " ^ (exnMessage e) ^ "\n");
                     {success=success, fail=fail, error=error + 1})
    )
  | (TestSuite (name, tests)) =>
    (foldr (fn (t, stat) => run_test_iter stat (path_name path name) t)
           results
           tests);
*)

fun intervalToString (dt : Time.time) : string =
    (LargeInt.toString (Time.toMicroseconds dt))^"ms";

(* The "path" refers to relative positioning of a test within the
   hierarchy of suites. It's use is for looking up failing tests.

   If a suite ends with a '/', then it is treated as a separator.

   Otherwise the default separator is a dot ".".

   Empty paths are treated as empty strings, and when we append a name
   to it, the path is "just" that new name.
 *)
fun path_name "" name = name
  | path_name path name = if String.isSuffix "/" path
                          then path ^ name
                          else (path ^ "." ^ name);

(* Print the results to the terminal *)
fun report_results path (ResultCase (name,dt,outcome)) =
    (case outcome of
         TestSuccess => print (concat ["Test ",
                                       (path_name path name),
                                       " (",
                                       intervalToString dt,
                                       ")",
                                       ": SUCCESS\n"])
       | (TestFail msg) => print (concat ["Test ",
                                          (path_name path name),
                                          " (",
                                          intervalToString dt,
                                          "): FAIL -- ",
                                          msg,
                                          "\n"])
       | (TestException e) => print (concat ["Test ",
                                             (path_name path name),
                                             " (",
                                             intervalToString dt,
                                             "): UNHANDLED EXCEPTION ",
                                             exnMessage e,
                                             "\n"]))
  | report_results path (ResultSuite (name,dt,outcomes)) =
    (print (concat ["Suite ",
                    (path_name path name),
                    " (", intervalToString dt, ")\n"]);
     app (report_results (path_name path name)) outcomes);


fun run_tests () = app ((report_results "") o run_test) (!all_tests);

type ResultSummary = {success : int, fail : int, errors : int};

val no_results = {success=0, fail=0, errors=0};

fun merge_summaries (({success=s1, fail=f1, errors=e1},
                     {success=s2, fail=f2, errors=e2}) : ResultSummary*ResultSummary)
    : ResultSummary
    = {success = s1+s2,
       fail = f1 + f2,
       errors = e1 + e2};

(* summarize : TestRest -> ResultSummary *)
fun summarize path (ResultCase (name, dt, outcome)) =
    (case outcome of
         TestSuccess => {success=1, fail=0, errors=0}
       | (TestFail msg) => (print (concat ["Test ",
                                           (path_name path name),
                                           " (",
                                           intervalToString dt,
                                           "): FAIL -- ",
                                           msg,
                                           "\n"]);
                            {success=0, fail=1, errors=0})
       | (TestException e) => (print (concat ["Test ",
                                             (path_name path name),
                                             " (",
                                             intervalToString dt,
                                             "): UNHANDLED EXCEPTION ",
                                             exnMessage e,
                                             "\n"]);
                               {success=0, fail=0, errors=1}))
  | summarize path (ResultSuite (name, _, outcomes)) =
    foldl merge_summaries
          no_results
          (map (summarize (path_name path name)) outcomes);


fun run_tests_short () =
    foldr merge_summaries
          no_results
          (map ((summarize "") o run_test) (!all_tests));

fun main () =
    run_tests();
