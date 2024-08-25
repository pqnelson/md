(*
SML/NJ requires putting everything into structures, so
when we have a test suite, we need to put it into a structure
for pointless bureacracy.
*)

signature SUITE = sig
  val suite : Test.t;
end;
