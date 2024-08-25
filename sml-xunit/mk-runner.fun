(*
Using any test reporter, we can run the tests, then
report the results.
*)
functor MkRunner(Reporter : REPORTER) :> RUNNER = struct
  fun run () =
    let
      val results = map Test.run (!Test.all);
      val summaries = foldr Test.merge Test.no_results
                            (map Test.summarize results);
    in
      map Reporter.report results;
      OS.Process.exit (Test.exit_status summaries);
      ()
    end;
end;

(*
fun main () =
    let
      val results = map run_test (!all_tests);
      val summaries = foldr merge_summaries no_results
                            (map summarize results);
      val report = JUnitTt.report;
    in
      app report results;
      OS.Process.exit (exit_status summaries);
      ()
    end;
*)
