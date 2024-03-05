(*
   dune build -w
   dune exec --no-print-directory ./plot.exe < data.txt
   dune exec --no-print-directory ./plot.exe < data2.txt
*)

module G = Graphics

let read_pair () =
  let x = read_int () in
  let y = read_int () in
  (x, y)
;;

let compare (x1, _) (x2, _) = x1 - x2

let main () =
  let n = read_int () in
  let data = Array.init n (fun _ -> read_pair ()) in
  let (x0, y0) = data.(0) in
  ()
  ; Array.sort compare data
  ; G.open_graph " 200x200"
  ; G.set_line_width 3
  ; G.moveto x0 y0
  ; for i = 1 to n - 1 do
      let (x, y) = data.(i) in
      G.lineto x y
    done
  ; print_endline "Enter a key to exit"
  ; G.read_key () |> ignore
;;

let () = main ()
