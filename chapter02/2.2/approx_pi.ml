(*

  echo approx_pi.ml | entr -cr bash -c 'ocamlc -o ./approx_pi.exe ./approx_pi.ml && ./approx_pi.exe 500'
  echo approx_pi.ml | entr -cr bash -c 'ocamlopt -o ./approx_pi.exe ./approx_pi.ml && ./approx_pi.exe 500'

  The perf difference between bytecode and native is drastic here:

  ======

  $ time ./approx_pi.exe 5000000 # BYTECODE
3.142197

real    0m3.606s
user    0m3.598s
sys     0m0.004s

======

$ time ./approx_pi.exe 5000000 # NATIVE
3.142197

real    0m0.253s
user    0m0.252s
sys     0m0.000s

======

To link the Unix package, I must compile the program with `ocamlfind` instead.

  echo approx_pi.ml | entr -cr bash -c 'ocamlfind opt -linkpkg -package unix -o ./approx_pi.exe ./approx_pi.ml && ./approx_pi.exe 500'

*)

let usage = Printf.sprintf "Usage: %s NUMBER" Sys.argv.(0)

type error = Err_missing_arg1 | Err_invalid_arg1

let err_dict err =
  match err with
  | Err_missing_arg1 -> (1, usage)
  | Err_invalid_arg1 -> (2, usage)

let die err () =
  let i, msg = err_dict err in
  print_endline msg;
  exit i

let n =
  let n' =
    try Sys.argv.(1) |> int_of_string_opt with
    | Invalid_argument _ -> die Err_missing_arg1 ()
  in
  match n' with
  | None -> die Err_invalid_arg1 ()
  | Some x -> x

(*

In Excel:

Col A        -> =RAND() over n lines...
Col B        -> =SQRT(1-An^2) over n line...
Col C Line 1 -> =4*AVERAGE(B:B) over 1 line

*)
let () =
  Random.init (Unix.gettimeofday () |> int_of_float);
  let p = ref 0 in
  for k = 1 to n do
    let x = Random.float 1. in
    let y = Random.float 1. in
    if (x *. x) +. (y *. y) <= 1. then p := !p + 1
  done;
  let pi = 4. *. float !p /. float n in
  Printf.printf "Looking for: %f\n" 3.1415927;
  Printf.printf "Got: %f\n\n" pi;
  for
    x =
      Printf.printf "*";
      1
    to Printf.printf ". <-- for conditions are evaluated only once\n\n";
       10
  do
    Printf.printf "Printing row %02d\n" x
  done
