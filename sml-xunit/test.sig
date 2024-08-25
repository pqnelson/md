signature TEST = sig
  type Test;
  type t = Test;
  datatype Outcome = Success
                   | Failure of string
                   | Exception of exn;

  type ResultSummary;
  datatype Result = ResultCase of string * Time.time * Outcome
                  | ResultSuite of string * Time.time * (Result list);

  val all : (Test list) ref;

  val suite : string -> Test list -> Test;
  val new : string -> (unit -> unit) -> Test;
  val register_suite : string -> Test list -> Test;
  val run : Test -> Result;
  val path : string -> string -> string;

  val no_results : ResultSummary;
  val merge : ResultSummary * ResultSummary -> ResultSummary;

  val summarize : Result -> ResultSummary;

  val exit_status : ResultSummary -> OS.Process.status;
end;
