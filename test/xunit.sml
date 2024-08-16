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

datatype TestResult
  = ResultCase of string * Time.time * TestOutcome
  | ResultSuite of string * Time.time * (TestResult list);

fun run_test (test : Test) : TestResult =
let
  val start = Time.now()
in
  case test of
      (TestCase (case_name, assertion)) =>
      ((assertion();
        ResultCase (case_name,
                    (Time.-)(Time.now(), start),
                    TestSuccess))
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

fun intervalToString (dt : Time.time) : string =
    (LargeInt.toString (Time.toMicroseconds dt))^"ms";

(*
The "path" refers to relative positioning of a test within the
hierarchy of suites. It's use is for looking up failing tests.

If a suite ends with a '/', then it is treated as a separator.

Otherwise the default separator is a dot ".".

Empty paths are treated as empty strings, and when we append a
name to it, the path is "just" that new name.
*)
fun path_name "" name = name
  | path_name path name = if String.isSuffix "/" path
                          then path ^ name
                          else (path ^ "." ^ name);

type ResultSummary = {success : int, fail : int, errors : int};

val no_results = {success=0, fail=0, errors=0};

fun merge_summaries (({success=s1, fail=f1, errors=e1},
                      {success=s2, fail=f2, errors=e2})
                     : ResultSummary*ResultSummary)
    : ResultSummary
    = {success = s1+s2,
       fail = f1 + f2,
       errors = e1 + e2};

(* summarize : TestRest -> ResultSummary *)
fun summarize (ResultCase (name, dt, outcome)) =
    (case outcome of
         TestSuccess => {success=1, fail=0, errors=0}
       | (TestFail msg) => {success=0, fail=1, errors=0}
       | (TestException e) => {success=0, fail=0, errors=1})
  | summarize (ResultSuite (name, _, outcomes)) =
    foldl merge_summaries
          no_results
          (map summarize outcomes);

(*
If an exceptional event occurred, the process failed.
*)
fun exit_status ({success,fail,errors} : ResultSummary) =
    if (0 = fail) andalso (0 = errors)
    then OS.Process.success
    else OS.Process.failure;

