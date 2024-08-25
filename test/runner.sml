(*

Using the different reporters, we can run the tests, then
report the results.

Defaults to the JUnitTt reporter.

For inspecting per test result, VerboseTt.report may be useful
*)
structure Runner :> RUNNER = MkRunner(JUnitTt);

val main = Runner.run;
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
