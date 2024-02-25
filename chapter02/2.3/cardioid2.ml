open Graphics

(*
   dune exec ./cardioid2.exe
*)

let () =
  (* Open graphics window. Space prefix is necessary. Origin is at the bottom left. *)
  open_graph " 300x200"
;;

(** Change the theta angle between 0 and 2PI
    Note that: 4 * atan 1 = PI

    In other words, between:

    atan(1) * (0 / 25) = 0
    atan(1) *       0  = 0

    And

    atan(1) * (200 / 25) = approx. 6.2831853
    atan(1) *         8  = approx. 6.2831853 *)
let new_theta n = atan 1.0 *. float n /. 25.0

let () =
  let radius_length = 50.0 in
  ()
  ; moveto 200 150 (* Set current point *)
  ; Unix.sleepf 0.5
  ; for i = 0 to 200 do
      let theta = new_theta i in
      let radius = radius_length *. (1.0 -. sin theta) in
      ()
      ; lineto (* Trace segment from current point to coordinate *)
          (150 + truncate (radius *. cos theta))
          (150 + truncate (radius *. sin theta))
      ; Unix.sleepf 0.008
    done
  ; Unix.sleepf 0.5
;;
