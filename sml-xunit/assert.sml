structure Assert :> ASSERT = struct
  exception Failure of string;

  fun !! msg is_success =
    if is_success then ()
    else raise Failure msg;

  fun eq (expected, actual, msg) =
    !! msg (expected = actual);
end;
