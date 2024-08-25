signature REPORTER = sig
    type t; (* type of the artifact produced *)
    val report : Test.Result -> t;
end;
