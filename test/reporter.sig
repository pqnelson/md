signature REPORTER = sig
    type t; (* type of the artifact produced *)
    val report : TestResult -> t;
end;
