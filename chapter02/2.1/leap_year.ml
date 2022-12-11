(*
  ocamlc -o leap_year.exe ./leap_year.ml

  # Run interactively
  ./leap_year.exe

  # Or pipe into STDIN
  $ echo 2022 | ./leap_year.exe
  2022 is not a leap year

*)
let year_from_stdin = read_int ()

let leap =
  (year_from_stdin mod 4 = 0 && year_from_stdin mod 100 <> 0)
  || year_from_stdin mod 400 = 0

let msg =
  if leap then
    "is"
  else
    "is not"

let () = Printf.printf "%d %s a leap year\n" year_from_stdin msg
