structure VerboseTt : REPORTER = struct
  type t = unit;

  fun intervalToString (dt : Time.time) : string =
    (LargeInt.toString (Time.toMicroseconds dt))^"ms";

  (* Print the results to the terminal *)
  fun report_results path (Test.ResultCase (name,dt,outcome)) =
    (case outcome of
         Test.Success => print (concat ["Test ",
                                        (Test.path path name),
                                        " (",
                                        intervalToString dt,
                                        ")",
                                        ": SUCCESS\n"])
       | (Test.Failure msg) => print (concat ["Test ",
                                              (Test.path path name),
                                              " (",
                                              intervalToString dt,
                                              "): FAIL -- ",
                                              msg,
                                              "\n"])
       | (Test.Exception e) => print (concat ["Test ",
                                              (Test.path path name),
                                              " (",
                                              intervalToString dt,
                                              "): UNHANDLED EXCEPTION ",
                                              exnMessage e,
                                              "\n"]))
    | report_results path (Test.ResultSuite (name,dt,outcomes)) =
      (print (concat ["Suite ",
                      (Test.path path name),
                      " (", intervalToString dt, ")\n"]);
       app (report_results (Test.path path name)) outcomes);
  
  val report = report_results "";

end;
