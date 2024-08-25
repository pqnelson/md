structure Test : TEST = struct
  datatype Test = Case of string*(unit -> unit)
                | Suite of string*(Test list);
  type t = Test;
  datatype Outcome = Success
                   | Failure of string
                   | Exception of exn;
  datatype Result = ResultCase of string * Time.time * Outcome
                  | ResultSuite of string * Time.time * (Result list);
  type ResultSummary = { success : int
                       , fail : int
                       , errors : int
                       };

  val all : (Test list) ref = ref [];

  fun suite name tests = Suite(name, tests);
  fun new name f = Case(name,f);
  fun register_suite name tests =
    let
      val s = suite name tests
    in
      all := s::(!all);
      s
    end;

  fun run (Case (name, assertion)) =
    let
      val start = Time.now();
    in
      assertion();
      ResultCase(name,
                 (Time.-)(Time.now(), start),
                 Success)
      handle Assert.Failure msg => ResultCase (name,
                                               (Time.-)(Time.now(), start),
                                               Failure msg)
           | e => ResultCase(name,
                             (Time.-)(Time.now(), start),
                             Exception e)
    end
    | run (Suite (name, tests)) =
      let
        val start = Time.now();
        val results = map run tests;
        val dt = (Time.-)(Time.now(), start);
      in
        ResultSuite (name, dt, results)
      end;
  
(*
The "path" refers to relative positioning of a test within the
hierarchy of suites. It's use is for looking up failing tests.

If a suite ends with a '/', then it is treated as a separator.

Otherwise the default separator is a dot ".".

Empty paths are treated as empty strings, and when we append a
name to it, the path is "just" that new name.
*)
  fun path "" name = name
    | path p name = if String.isSuffix "/" p
                    then p ^ name
                    else (p ^ "." ^ name);

  val no_results = { success = 0
                   , fail = 0
                   , errors = 0
                   };

  fun merge ({success=s1,fail=f1,errors=e1}
            ,{success=s2,fail=f2,errors=e2}) =
    { success = s1 + s2
    , fail = f1 + f2
    , errors = e1 + e2
    };

  fun summarize (ResultCase (_, _, outcome)) =
    (case outcome of
         Success => {success=1,fail=0,errors=0}
       | (Failure _) => {success=0,fail=1,errors=0}
       | (Exception _) => {success=0,fail=0,errors=1})
    | summarize (ResultSuite (_, _, outcomes)) =
      foldl merge
            no_results
            (map summarize outcomes);

  fun exit_status ({success, fail, errors}) =
    if (0 = fail) andalso (0 = errors)
    then OS.Process.success
    else OS.Process.failure;
end;
