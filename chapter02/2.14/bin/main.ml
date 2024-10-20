(*
   dune exec --no-print-directory --display=quiet bin/main.exe -w
*)

module IntSet = Set.Make (struct
    type t = int

    let compare = compare
  end)

let pp_set a b = Fmt.(concat ~sep:(any "") [ any "{"; iter ~sep:(any " ") a b; any "}" ])
let pp_int_set = pp_set IntSet.iter Fmt.int

(* Book version. I found its API a little weird, it requires starting with an `n-1` input. *)
let rec upto_book n =
  if n < 0 then
    IntSet.empty
  else
    IntSet.add n (upto_book (n - 1))
;;

(* My version *)
let upto n =
  Seq.unfold (fun n -> Some (n, n + 1)) 0
  |> Seq.take n
  |> IntSet.of_seq
[@@ocamlformat "disable"]

(*
    
*)

(* Book version *)
let rec possible_solutions cols d1 d2 =
  (* let () =
     let co = cols in
     let cr = IntSet.diff (IntSet.diff cols d1) d2 in
     ()
     ; print_string (Fmt.str "@[co = %-20s@]| " (Fmt.str "%a" pp_int_set co))
     ; print_string (Fmt.str "@[d1 = %-20s@]| " (Fmt.str "%a" pp_int_set d1))
     ; print_string (Fmt.str "@[d2 = %-20s@]| " (Fmt.str "%a" pp_int_set d2))
     (* ; print_string (Fmt.str "@[cn = %-20s@]| " (Fmt.str "%a" pp_int_set cr)) *)
     ; print_newline ()
     in *)
  if IntSet.is_empty cols then
    1
  else
    IntSet.fold
      (fun c res ->
        let d1 = IntSet.map succ (IntSet.add c d1) in
        let d2 = IntSet.map pred (IntSet.add c d2) in
        res + possible_solutions (IntSet.remove c cols) d1 d2)
      (IntSet.diff (IntSet.diff cols d1) d2)
      0
;;

let main () =
  ()
  ; Fmt.pr "Book) %a\n%!" pp_int_set (upto_book (8 - 1))
  ; Fmt.pr "Mine) %a\n%!" pp_int_set (upto 8)
  ; Fmt.pr "C) %a\n%!" pp_int_set (IntSet.diff (upto 8) (upto 9))
  ; Fmt.pr "D) %a\n%!" pp_int_set (IntSet.diff (upto 9) (upto 8))
  ; Fmt.pr
      "Possible solutions for the state given in the book: %d\n%!"
      (possible_solutions
         (IntSet.empty
          |> IntSet.add 0
          |> IntSet.add 2
          |> IntSet.add 5
          |> IntSet.add 6
          |> IntSet.add 7)
         (IntSet.empty |> IntSet.add 3 |> IntSet.add 5 |> IntSet.add 6)
         (IntSet.empty |> IntSet.add 0 |> IntSet.add 3))
  ; Fmt.pr
      "Possible solutions for 8 queens: %d\n%!"
      (possible_solutions (upto 8) IntSet.empty IntSet.empty)
  ; Fmt.pr
      "Possible solutions for 14 queens: %d\n%!"
      (possible_solutions (upto 14) IntSet.empty IntSet.empty)
  ; print_endline "Done!"
;;

(*
   [2.14]$ time _build/default/bin/
   .main.eobjs/  main.exe      .merlin-conf/
   [2.14]$ time _build/default/bin/main.exe
   Testing: 365596

   real    0m19,175s
   user    0m19,140s
   sys     0m0,008s
*)
let test () =
  ()
  ; Fmt.pr
      "Testing: %d\n%!"
      (possible_solutions (IntSet.map succ @@ upto 14) IntSet.empty IntSet.empty)
;;

test ()
