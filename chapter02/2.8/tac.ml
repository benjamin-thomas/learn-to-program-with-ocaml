(*
   rg --files | entr -c bash -c "VERSION=0 dune exec --no-print-directory --display=quiet ./tac.exe < ./input.txt"
   VERSION=0 dune exec --no-print-directory --display=quiet ./tac.exe < <(seq 10)
*)

module type VERSION = sig
  val run : unit -> unit
end

module BookVersion : VERSION = struct
  let lines = ref []

  let init () =
    try
      while true do
        lines := read_line () :: !lines
      done
    with
    | End_of_file -> ()
  ;;

  let rec length = function
    | [] -> 0
    | _ :: t -> 1 + length t
  ;;

  let rec print = function
    | [] -> ()
    | h :: t ->
      print_endline h;
      print t
  ;;

  let run () =
    init ();
    print !lines;
    Printf.printf "%d lines read\n" (length !lines);
    ()
  ;;
end

module V1 : VERSION = struct
  let consume_stdin () =
    let rec aux (counter, lines) =
      try aux (counter + 1, read_line () :: lines) with
      | End_of_file -> (counter, lines)
    in
    aux (0, [])
  ;;

  let run () =
    let (count, lines) = consume_stdin () in
    List.iter print_endline lines;
    Printf.printf "%d lines read\n" count
  ;;
end

module V2 : VERSION = struct
  let consume_stdin =
    Seq.unfold (fun () ->
      try
        let line = read_line () in
        Some (line, ())
      with
      | End_of_file -> None)
  ;;

  let run () =
    let (count, lines) =
      Seq.fold_left
        (fun (counter, lines) line -> (counter + 1, line :: lines))
        (0, [])
        (consume_stdin ())
    in
    List.iter print_endline lines;
    Printf.printf "%d lines read\n" count;
    ()
  ;;
end

let () =
  match Sys.getenv_opt "VERSION" with
  | Some "0" -> BookVersion.run ()
  | Some "1" -> V1.run ()
  | Some "2" -> V2.run ()
  | _ -> print_endline "Required env var: VERSION=0|1|2|3"
;;
