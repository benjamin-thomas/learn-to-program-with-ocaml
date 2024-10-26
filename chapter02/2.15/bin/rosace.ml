(*
   dune exec --no-print-directory --display=quiet bin/rosace.exe
*)

module G = Graphics

let () =
  G.open_graph " 800x800";
  G.set_window_title "Rosace";
  let pi = 4. *. atan 1. in
  let center_x = 400. in
  let center_y = 400. in
  let offset = 100. in
  let circle_size = 100 in
  let circle n =
    G.draw_circle
      (truncate @@ (center_x +. (offset *. cos (pi *. n))))
      (truncate @@ (center_y +. (offset *. sin (pi *. n))))
      circle_size
  in
  G.draw_circle 400 400 circle_size;
  circle @@ (1. /. 3.);
  circle @@ (2. /. 3.);
  circle @@ (3. /. 3.);
  circle @@ (4. /. 3.);
  circle @@ (5. /. 3.);
  circle @@ (6. /. 3.);
  input_line stdin |> ignore
;;
