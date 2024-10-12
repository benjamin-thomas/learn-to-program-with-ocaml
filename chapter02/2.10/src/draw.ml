module G = Graphics
open Printf

let left = 0.
let right = 300.
let down = 0.
let up = 200.
let ball = 5
let paddle = 50
let thick = 8

let gray =
  let n = 220 in
  G.rgb n n n
;;

let init () =
  let s = sprintf " %dx%d" (truncate right) (truncate up) in
  G.open_graph s;
  G.auto_synchronize false;
  ()
;;

let clear () =
  G.set_color gray;
  G.fill_rect 0 0 (truncate right) (truncate up)
;;

let get_paddle_pos () =
  let x = fst (G.mouse_pos ()) in
  max 0 (min x (truncate right - paddle))
;;

let game x y =
  clear ();
  G.set_color G.black;
  G.fill_circle (truncate x) (truncate y) ball;
  let x = get_paddle_pos () in
  G.fill_rect x 0 paddle thick;
  G.synchronize ();
  x
;;
