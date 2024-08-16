structure VerboseTt : REPORTER = struct
  type t = unit;

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

  val report = report_results "";
end;
