
(* 
Print to the terminal (hence the "-Tt" suffix) a summary of test
results in the style of JUnit.

If any failures or errors occur, print those out with a bit more
detail.
*)
structure JUnitTt : REPORTER = struct
  type t = unit;

  fun intervalToString (dt : Time.time) : string =
    (LargeInt.toString (Time.toMicroseconds dt))^"ms";
  
  fun count_tests_run ({success, fail, errors} : Test.ResultSummary)
    = success + fail + errors;

  fun report_iter path (Test.ResultCase (name, dt, outcome) : Test.Result) =
    (case outcome of
         (Test.Failure msg) => print ((Test.path path name) ^
                                      " FAIL: " ^ msg ^ "\n")
       | (Test.Exception e) => print ((Test.path path name) ^
                                      " ERROR: " ^
                                      (exnMessage e) ^
                                      "\n")
       | _ => ())
    | report_iter path (r as (Test.ResultSuite (name, dt, results))) =
      let
        val summary = Test.summarize r;
        val new_path = Test.path path name;
      in
        (print ("Running "^new_path^"\n");
         app (report_iter new_path) results;
         print (concat ["Tests run: ",
                        Int.toString (count_tests_run summary),
                        ", ",
                        "Failures: ",
                        Int.toString (#fail summary),
                        ", ",
                        "Errors: ",
                        Int.toString (#errors summary),
                        ", ",
                        (* "Skipped: ", #skipped summary, " ", *)
                        "Time elapsed: ", intervalToString dt,
                        " - in ", new_path,
                        "\n"]))
      end;
  
  val report = report_iter "";
end;
