(*
  ocamlc -o leap_year_flex.exe ./leap_year_flex.ml

  # Run interactively
  ./leap_year_flex.exe 2022

  # Or pipe into STDIN
  $ echo 2022 | ./leap_year_flex.exe
  2022 is not a leap year

  ======

  # `ocamlc -i` prints the variable types.
  $ ocamlc -i ./leap_year_flex.ml
  val year_from_stdin_or_argv : int
  val leap : bool
  val msg : string

  # Can be useful that way
  ocamlc -i ./leap_year_flex.ml > ./leap_year_flex.mli

*)

let year_from_stdin_or_argv =
  if Array.length Sys.argv = 2 then
    int_of_string Sys.argv.(1)
  else
    read_int ()

let leap =
  (year_from_stdin_or_argv mod 4 = 0 && year_from_stdin_or_argv mod 100 <> 0)
  || year_from_stdin_or_argv mod 400 = 0

let msg =
  if leap then
    "is"
  else
    "is not"

let () = Printf.printf "%d %s a leap year\n" year_from_stdin_or_argv msg
