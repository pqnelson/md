
(* 
Print to the terminal (hence the "-Tt" suffix) a summary of test
results in the style of JUnit.

If any failures or errors occur, print those out with a bit more
detail.
*)
structure JUnitTt : REPORTER = struct
  type t = unit;

  fun count_tests_run ({success, fail, errors} : ResultSummary)
      = success + fail + errors;

  fun report_iter path (ResultCase (name, dt, outcome)) =
      (case outcome of
           (TestFail msg) => print ((path_name path name) ^
                                    " FAIL: " ^ msg ^ "\n")
         | (TestException e) => print ((path_name path name) ^
                                       " ERROR: " ^
                                       (exnMessage e) ^
                                       "\n")
         | _ => ())
  | report_iter path (r as (ResultSuite (name, dt, results))) =
    let
      val summary = summarize r;
      val new_path = path_name path name;
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
