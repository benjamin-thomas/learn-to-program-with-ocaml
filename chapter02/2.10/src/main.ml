(*
   dune exec --no-print-directory --display=quiet src/main.exe -w
*)

let bounce (x, y) (vx, vy) xp =
  let vx = if x <= Draw.left || x >= Draw.right then -.vx else vx in
  let vy =
    if (y <= float Draw.thick && x >= xp && x <= xp +. float Draw.paddle) || y >= Draw.up
    then -.vy
    else vy
  in
  vx, vy
;;

let new_pos (x, y) (vx, vy) = x +. vx, y +. vy

let rec play (x, y) (vx, vy) =
  if y <= Draw.down then failwith "game over";
  let xp = Draw.game x y in
  let vx, vy = bounce (x, y) (vx, vy) (float xp) in
  let x', y' = new_pos (x, y) (vx, vy) in
  play (x', y') (vx, vy)
;;

let () =
  Draw.init ();
  let speed = 0.01 in
  let vx = speed *. Random.float 1. in
  let vy = speed *. Random.float 1. in
  play (Draw.right /. 2., float Draw.thick) (vx, vy)
;;
