(*
   rg --files | entr -rc bash -c 'dune exec --no-print-directory --display=quiet chapter02/2.9/OCaml/radix_mine.exe 16 < <(echo -e "ABC\ndef\nAz\nFF")'

   ---

   I can even convert to a base 36! (usage of 0-9 + A-Z)

   dune exec --no-print-directory --display=quiet chapter02/2.9/OCaml/radix_mine.exe 36 < <(echo -e "z\n10")
   z
   35  -> 35

   10
   0 1  -> 36
*)

open Printf

let red = "\027[31m"
let reset = "\027[0m"
let usage pgm = sprintf "Usage: %s BASE" (Filename.basename pgm)

let die msg =
  print_endline msg;
  exit 1
;;

let base10_of_char c =
  match c with
  | '0' .. '9' -> Some (Char.code c - Char.code '0')
  | 'A' .. 'Z' -> Some (Char.code c - (Char.code 'A' - 10))
  | 'a' .. 'z' -> Some (Char.code c - (Char.code 'a' - 10))
  | _ -> None
;;

let convert_to_base10 base =
  List.fold_left
    begin
      fun (good, bad) c ->
        match base10_of_char c with
        | None -> (good, c :: bad)
        | Some n ->
          if n < 0 || n >= base then
            (good, c :: bad)
          else
            (n :: good, bad)
    end
    ([], [])
;;

let horner_rev base lst = List.fold_right (fun n acc -> (base * acc) + n) lst 0

let handle_line base line =
  let chars = List.init (String.length line) (String.get line) in
  let () = print_endline line in
  match convert_to_base10 base chars with
  | (good, []) ->
    List.iter
      (fun n ->
        print_int n;
        print_char ' ')
      good;
    let result = horner_rev base good in
    printf " -> %d\n\n" result
  | (_, bad) ->
    print_string red;
    print_string "  Bad chars: ";
    List.iter print_char (List.rev bad);
    print_string reset;
    print_newline ();
    print_newline ();
    ()
;;

let rec main_loop base =
  try
    let line = read_line () in
    handle_line base line;
    main_loop base;
    ()
  with
  | End_of_file -> ()
;;

let start pgm args =
  match args with
  | [] -> die @@ usage pgm
  | [ base ] ->
    let base =
      base
      |> int_of_string_opt
      |> function
      | None -> die @@ "Not a numeric value: " ^ base
      | Some x -> x
    in
    main_loop base
  | _ ->
    print_endline "Too many parameters";
    print_endline @@ usage pgm
;;

let () =
  match Array.to_list Sys.argv with
  | [] -> assert false
  | pgm :: args -> start pgm args
;;
