(*
 Run as:
    echo ./Primes.fsx | entr -c dotnet fsi /_

 Or load in the REPL
   $ dotnet fsi
   > #load "./Primes.fsx";;
   > open Primes;;
   > primes |> Seq.filter (fun n -> n > 100) |> Seq.take 4;;

 *)

let isPrime n =
    let rec check i =
        i > n / 2 || (n % i <> 0 && check (i + 1))

    check 2

let nats = Seq.initInfinite id
let primes = Seq.filter isPrime nats

let () = primes |> Seq.take 9 |> Seq.iter (printf "%d\n")
