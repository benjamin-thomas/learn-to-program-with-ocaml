open Printf

(*
   dune exec --no-print-directory ./sieve_of_eratosthenes.exe -w

   Also see: ./primes.ml
*)

let get_max () =
  ()
  ; print_string "Enter a number: "
  ; read_int ()
;;

let compute max prime =
  let limit = truncate @@ sqrt @@ float max in
  ()
  ; prime.(0) <- false
  ; prime.(1) <- false
  ; for n = 2 to limit do
      if prime.(n) then (
        let m = ref (n * n) in
        while !m <= max do
          ()
          ; prime.(!m) <- false
          ; m := !m + n
        done
      )
    done
;;

let () =
  let max =
    let rl = Sys.getenv_opt "RL" |> Option.value ~default:"0" in
    if rl = "1" then
      get_max ()
    else
      25
  in
  let prime = Array.make (max + 1) true in
  ()
  ; compute max prime
  ; for n = 2 to max do
      if prime.(n) then printf "%d\n" n
    done
;;
