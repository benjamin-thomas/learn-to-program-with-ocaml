(*
  ocamlc -o leap_year_param.exe ./leap_year_param.ml

  # Run interactively
  ./leap_year_param.exe 2022

*)
let year_from_argv = int_of_string Sys.argv.(1)

let leap =
  (year_from_argv mod 4 = 0 && year_from_argv mod 100 <> 0)
  || year_from_argv mod 400 = 0

let msg =
  if leap then
    "is"
  else
    "is not"

let () =
  Printf.printf "%d\n\n" @@ Array.length Sys.argv;
  Printf.printf "%d %s a leap year\n" year_from_argv msg
