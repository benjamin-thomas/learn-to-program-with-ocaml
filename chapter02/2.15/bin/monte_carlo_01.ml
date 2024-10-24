(*
   dune exec --no-print-directory --display=quiet bin/monte_carlo_01.exe -w

   ---

   The area of a circle is: PI × R²

   Since 1² = 1, the area of a circle of R=1 is thus equal to PI:

   So we draw random points on a 1x1 square area, where 1 represents the radius.

   If the point at (x² + y²) is <= 1, that means it's inside 1/4 of the circle,
   because we are inside the "unit circle" coordinates: https://www.mathsisfun.com/geometry/unit-circle.html

   (sqrt(2) / 2)² + (sqrt(2) / 2)² = 1
   (1 / 2)²       + (sqrt(3) / 2)² = 1
   (sqrt(3) / 2)² + (1 / 2)²       = 1
   etc.
*)

module G = Graphics
open Printf

let draw_quarter_circle size =
  G.set_line_width 4;
  G.set_color G.red;
  (* G.draw_circle 0 0 size *)
  G.draw_arc 0 0 size size 0 90
;;

let draw_dot ~is_inside ~on_inside ~x ~y =
  if is_inside then (
    G.set_color G.red;
    on_inside ()
  ) else
    G.set_color G.blue;
  G.fill_circle (int_of_float x) (int_of_float y) 1
;;

let () =
  let total_points = 160_000 in
  let wsize = 800.0 in
  let wsize' = int_of_float wsize in
  let total_inside = ref 0 in
  Random.self_init ();
  G.open_graph @@ sprintf " %dx%d" wsize' wsize';
  for _ = 1 to total_points do
    let x = Random.float 1.0 in
    let y = Random.float 1.0 in
    draw_quarter_circle wsize';
    draw_dot
      ~is_inside:((x *. x) +. (y *. y) <= 1.0)
      ~on_inside:(fun () -> total_inside := !total_inside + 1)
      ~x:(x *. wsize)
      ~y:(y *. wsize)
  done;
  printf "Looking for : %f\n%!" 3.1415927;
  let pi_approx = 4. *. float_of_int !total_inside /. float_of_int total_points in
  printf "PI is around: %f\n%!" pi_approx
;;
