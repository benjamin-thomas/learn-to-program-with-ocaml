(*
   dune exec --no-print-directory ./primes.exe -w

   Also see: ./sieve_of_eratosthenes.ml
*)

[@@@warning "-32"]

module Via_seq = struct
  let rec make_primes (seq : int Seq.t) : int Seq.t =
    match seq () with
    | Seq.Nil -> fun () -> Seq.Nil
    | Seq.Cons (h, seq) ->
      let next_primes = Seq.filter (fun n -> n mod h <> 0) seq in
      fun () -> Cons (h, make_primes next_primes)
  ;;

  let primes : int Seq.t = make_primes @@ Seq.ints 2

  let main () =
    print_string
    @@ String.concat " "
    @@ List.map string_of_int
    @@ List.of_seq
    @@ Seq.take 20 primes
  ;;
end

module Via_own = struct
  (** A lazy list represents an infinite list.
      It has no tail, but a "tail function".
      It has no `Nil` constructor because the list has no end. *)
  type 'a lazylist = Cons of 'a * (unit -> 'a lazylist)

  (** Builds a lazylist from [n], always increasing by 1 *)
  let rec lseq n = Cons (n, fun () -> lseq (n + 1))

  let rec unfold f x =
    let (h, h') = f x in
    Cons (h, fun () -> unfold f h')
  ;;

  let ints = unfold (fun n -> (n, n + 1))
  let nats = ints 0
  let fibs = unfold (fun (a, b) -> (a, (b, a + b))) (0, 1)
  let fibs = Seq.unfold (fun (a, b) -> Some (a, (b, a + b))) (0, 1)
  let ints = Seq.unfold (fun n -> Some (n, n + 1))
  let nats = Seq.ints 0

  let to_seq lst =
    Seq.unfold
      (function
        | [] -> None
        | h :: t -> Some (h, t))
      lst
  ;;

  let make_evens n = Cons (n, fun () -> lseq (n + 2))
  let evens = make_evens (-2)

  let rec ltake n (Cons (h, tf)) =
    if n > 0 then
      h :: ltake (n - 1) (tf ())
    else
      []
  ;;

  let rec lfilter test (Cons (h, tf)) : 'a lazylist =
    if test h then
      Cons (h, fun () -> lfilter test (tf ()))
    else
      lfilter test (tf ())
  ;;

  let rec make_primes (Cons (h, tf)) =
    let next_primes = lfilter (fun n -> n mod h <> 0) (tf ()) in
    Cons (h, fun () -> make_primes next_primes)
  ;;

  let primes : int lazylist = make_primes (lseq 2)

  let main () =
    print_string @@ String.concat " " @@ List.map string_of_int @@ ltake 20 primes
  ;;
end

let () =
  ()
  ; print_endline "Primes (via Seq module)"
  ; Via_seq.main ()
  ; print_newline ()
  ; print_newline ()
  ; print_endline "Primes (via own lazylist type)"
  ; Via_own.main ()
  ; print_newline ()
;;
