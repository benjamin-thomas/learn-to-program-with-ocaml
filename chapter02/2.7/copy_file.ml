(*
   dune exec ./chapter02/2.7/copy_file.exe /usr/share/dict/words /tmp/words
*)

let copy_file f1 f2 =
  let ic = open_in f1
  and oc = open_out f2 in
  try
    while true do
      output_char oc (input_char ic)
    done
  with
  | End_of_file ->
    ()
    ; close_in ic
    ; close_out oc
;;

let () = copy_file Sys.argv.(1) Sys.argv.(2)
