module G = Graphics
open Printf

let width = 800
let height = 800

(** [normalize n total] returns the normalized value of [n] in a [-2, -2] to [2, 2] coordinate system.

    See the "tic-tac-toe" board at: https://www.wikihow.com/Plot-the-Mandelbrot-Set-By-Hand *)
let normalize n total = (4. *. float_of_int n /. float_of_int total) -. 2.
[@@warning "-32"]
;;

let normalize_scaled n total center scale =
  (4. *. scale *. (float_of_int n /. float_of_int total))
  -. (2. *. scale)
  -. (center *. scale /. 2.)
;;

(* This isn't quite right *)
let zoom_to_click x y scale (center_x, center_y) =
  let scale = scale *. 0.8 in
  ( scale
  , (normalize_scaled x width center_x scale, normalize_scaled y height center_y scale) )
;;

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

let draw iterations scale (center_x, center_y) =
  let board = Array.make_matrix height width 0 in
  for x = 0 to width - 1 do
    for y = 0 to height - 1 do
      let a = normalize_scaled x width center_x scale in
      let b = normalize_scaled y height center_y scale in
      let remain = mandelbrot iterations (a, b) in
      let color = truncate @@ (float remain /. float iterations *. float G.white) in
      board.(y).(x) <- color
    done
  done;
  G.draw_image (G.make_image board) 0 0;
  G.moveto 10 5;
  G.draw_string @@ sprintf "Iterations: %d" iterations;
  caption "press '+' to increase iterations, '-' to decrease" 1;
  caption "press 'q' to quit" 2;
  ()
;;

let rec loop iterations scale (center_x, center_y) =
  draw iterations scale (center_x, center_y);
  let rec listen_keyboard () =
    let evt = G.wait_next_event [ Key_pressed; Button_down ] in
    match evt with
    | { G.button = true; mouse_x = x; mouse_y = y; _ } ->
      let (scale, (center_x, center_y)) = zoom_to_click x y scale (center_x, center_y) in
      loop iterations scale (center_x, center_y)
    | { G.key = '+'; _ } -> loop (iterations + 1) scale (center_x, center_y)
    | { G.key = '-'; _ } -> loop (iterations - 1) scale (center_x, center_y)
    | { G.key = '\224'; _ } ->
      loop iterations (scale *. 0.8) (center_x, center_y) (* Zoom in *)
    | { G.key = 'y'; _ } ->
      loop iterations (scale /. 0.8) (center_x, center_y) (* Zoom out *)
    | { G.key = 'u'; _ } ->
      loop iterations scale (center_x, center_y -. (scale *. 0.2)) (* up *)
    | { G.key = '\233'; _ } ->
      loop iterations scale (center_x, center_y +. (scale *. 0.2)) (* down *)
    | { G.key = 'a'; _ } ->
      loop iterations scale (center_x +. (0.2 *. scale), center_y) (* left *)
    | { G.key = 'i'; _ } ->
      loop iterations scale (center_x -. (0.2 *. scale), center_y) (*  right *)
    | { G.key = 'r'; _ } -> loop 15 1.0 (1., 1.)
    | { G.key = 'q'; _ } -> G.close_graph ()
    | _ ->
      printf "key=%d\n%!" @@ int_of_char evt.key;
      listen_keyboard ()
  in
  listen_keyboard ()
;;

let () =
  G.open_graph @@ sprintf " %dx%d" width height;
  G.set_window_title "Mandelbrot";
  loop 15 1.0 (1., 1.)
;;
