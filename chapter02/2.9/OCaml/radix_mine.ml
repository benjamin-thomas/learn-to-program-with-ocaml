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
  Char.(
    match c with
    | '0' .. '9' -> Some (code c - code '0')
    | 'A' .. 'Z' -> Some (code c - (code 'A' - 10))
    | 'a' .. 'z' -> Some (code c - (code 'a' - 10))
    | _ -> None)
;;

let convert_to_base10 base =
  List.fold_left
    begin
      fun (good, bad) c ->
        match base10_of_char c with
        | None -> (good, c :: bad)
        | Some n ->
          if n < 0 || n >= base || base > 36 || base < 2 then
            (good, c :: bad)
          else
            (n :: good, bad)
    end
    ([], [])
;;

let horner_rev base lst = List.fold_right (fun n acc -> (base * acc) + n) lst 0

let print_int_sep n =
  print_int n;
  print_char ' '
;;

let string_of_chars ~sep chars =
  let buf = Buffer.create 16 in
  List.iteri
    (fun i c ->
      if i != 0 then Buffer.add_char buf sep;
      Buffer.add_char buf c)
    chars;
  Buffer.contents buf
;;

let print_good chars value =
  List.iter print_int_sep chars;
  printf "-> %d\n\n" value
;;

let print_bad bad =
  print_string
  @@ red
  ^ " Bad chars: "
  ^ "["
  ^ string_of_chars ~sep:',' (List.rev bad)
  ^ "]"
  ^ reset
  ^ "\n\n"
;;

let handle_line base line =
  print_endline line;
  let chars = List.init (String.length line) (String.get line) in
  match convert_to_base10 base chars with
  | (good, []) -> print_good good (horner_rev base good)
  | (_, bad) -> print_bad bad
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
