(*
   dune exec ./chapter02/2.7/deserialize.exe /tmp/data.bin

   NOTE: this technique is sensitive to the OCaml version. Can generate seg faults!
*)
let read : string -> (int * string) Seq.t =
  fun path ->
  let ic = open_in path in
  let gen_row () =
    try Some (input_value ic, ()) with
    | End_of_file -> None
  in
  Seq.unfold gen_row ()
;;

let read_alt : string -> (int * string) list =
  fun path ->
  In_channel.with_open_bin path
  @@ fun ic ->
  let lines = ref [] in
  let () =
    try
      while true do
        lines := input_value ic :: !lines
      done
    with
    | End_of_file -> ()
  in
  List.rev !lines
;;

let () =
  let open Printf in
  ()
  ; print_newline ()
  ; print_endline "V1"
  ; read "/tmp/data.bin" |> Seq.iter (fun (num, str) -> printf "%d %s\n" num str)
  ; print_newline ()
  ; print_endline "V2"
  ; read_alt "/tmp/data.bin" |> List.iter (fun (num, str) -> printf "%d %s\n" num str)
  ; print_endline "DONE! (v2)"
;;
