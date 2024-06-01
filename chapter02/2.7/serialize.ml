(*
   dune exec ./chapter02/2.7/serialize.exe
*)
let data = [ (1, "AAA"); (2, "BBB"); (3, "CCC") ]

let write path =
  Out_channel.with_open_bin path
  @@ fun oc ->
  List.iter (output_value oc) data
[@@ocamlformat "disable"]

let () =
  ()
  ; write "/tmp/data.bin"
  ; print_endline "DONE!"
;;
