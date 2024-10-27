module G = Graphics
open Printf

let width = 800
let height = 800

(** [normalize n total] returns the normalized value of [n] in a [-2, -2] to [2, 2] coordinate system.

    See the "tic-tac-toe" board at: https://www.wikihow.com/Plot-the-Mandelbrot-Set-By-Hand *)
let normalize n total = (4. *. float_of_int n /. float_of_int total) -. 2.

let magnitude x y = (x *. x) +. (y *. y)

let mandelbrot iter (a, b) =
  let rec aux remaining (x, y) =
    if remaining = 0 || magnitude x y > 4. then
      remaining
    else (
      let x = (x *. x) -. (y *. y) +. a
      and y = (2. *. x *. y) +. b in
      aux (remaining - 1) (x, y)
    )
  in
  aux iter (0.0, 0.0)
;;

let caption str line =
  G.moveto (width - (fst @@ G.text_size str) - 10) (height - (line * 15));
  G.draw_string str
;;

let draw iterations =
  let board = Array.make_matrix height width 0 in
  for x = 0 to width - 1 do
    for y = 0 to height - 1 do
      let a = normalize x width in
      let b = normalize y height in
      let remain = mandelbrot iterations (a, b) in
      let color = truncate @@ (float remain /. float iterations *. float G.white) in
      board.(y).(x) <- color
    done
  done;
  G.draw_image (G.make_image board) 0 0;
  (* Calling `moveto` ensures the text doesn't get draw off screen since the cursor moves upon each call *)
  G.moveto 10 5;
  G.draw_string @@ sprintf "Iterations: %d" iterations;
  caption "press '+' to increase iterations" 1;
  caption "press 'q' to quit" 2;
  ()
;;

let rec loop iterations =
  draw iterations;
  let rec listen_keyboard () =
    let evt = G.wait_next_event [ Key_pressed ] in
    match evt.key with
    | '+' -> loop (iterations + 1)
    | '-' -> loop (iterations - 1)
    | 'q' -> G.close_graph ()
    | _ -> listen_keyboard ()
  in
  listen_keyboard ()
;;

let () =
  G.open_graph @@ sprintf " %dx%d" width height;
  G.set_window_title "Mandelbrot";
  loop 15
;;
