(*
   dune exec --no-print-directory ./primes2.exe && echo
*)

open Printf

let primes =
  let is_prime n =
    let rec check i = i > n / 2 || (n mod i <> 0 && check (i + 1)) in
    check 2
  in
  Seq.filter is_prime (Seq.ints 2)
;;

let () = Seq.iter (printf "%d ") @@ Seq.take 9 primes
