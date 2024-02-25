(*
   dune exec ./approx_pi2.exe
*)

open Printf

let usage = sprintf "Usage: %s NUMBER" Sys.argv.(0)

let get_arg () =
  try Sys.argv.(1) |> int_of_string with
  | _ ->
    ()
    ; print_string usage
    ; print_newline ()
    ; exit 1
;;

(*
   In Excel:

   Col A        -> =RAND() over n lines...
   Col B        -> =SQRT(1-An^2) over n line...
   Col C Line 1 -> =4*AVERAGE(B:B) over 1 line
*)

let compute_pi n =
  let p = ref 0 in
  ()
  ; for _ = 1 to n do
      let x = Random.float 1. in
      let y = Random.float 1. in
      if (x *. x) +. (y *. y) <= 1.0 then p := !p + 1
    done
  ; 4.0 *. float !p /. float n
;;

let () =
  let n = get_arg () in
  ()
  ; Random.init (int_of_float @@ Unix.gettimeofday ())
  ; printf "Looking for: %f\n" 3.1415927
  ; printf "Got: %f\n\n" (compute_pi n)
;;
