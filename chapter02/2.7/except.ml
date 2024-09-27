(* Catch an exception *)
let[@warning "-52"] not_quite_rand_num () =
  try Random.int (-1) (* throws *) with
  | Invalid_argument "Random.int" -> 0
;;

open Printf

exception Stop of int

let rec work n =
  assert (n > 0)
  ; printf "%2d) Doing stuff...\n" n
  ; if n >= 3 then
      raise (Stop n)
    else
      work (n + 1)
;;

let defend n =
  if n < 0 then
    (* Fatal error: exception Invalid_argument("negative number") *)
    invalid_arg "negative number"
  else if n = 0 then
    (* Fatal error: exception Failure("zero is forbidden too") *)
    failwith "zero is forbidden too"
  else (
    (* Fatal error: exception Assert_failure("chapter02/2.7/except.ml", 29, 6) *)
    ()
    ; assert (n > 1)
    ; print_endline "Ok, I'll carry on..."
  )
;;

let () =
  ()
  ; print_newline ()
  ; printf "\"Random\" value: %d\n\n" @@ not_quite_rand_num ()
  ; (try work 1 with
     | Stop n -> printf "\nGame over! (stopped at: %d)\n" n)
  ; defend 1
;;
