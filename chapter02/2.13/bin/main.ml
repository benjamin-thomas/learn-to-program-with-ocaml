(* dune exec --no-print-directory --display=quiet bin/main.exe -w *)

type quad =
  | White
  | Black
  | Node of quad * quad * quad * quad

let rec checker_board = function
  | 0 -> Black
  | 1 -> Node (White, Black, White, Black)
  | n ->
    let q = checker_board (n - 1) in
    Node (q, q, q, q)
;;

module G = Graphics

let rec draw x y w = function
  | White -> ()
  | Black -> G.fill_rect x y w w
  | Node (q1, q2, q3, q4) ->
    let w = w / 2 in
    ()
    ; draw (x + 0) (y + 0) w q1 (* bottom left  *)
    ; draw (x + w) (y + 0) w q2 (* bottom right *)
    ; draw (x + w) (y + w) w q3 (* top right    *)
    ; draw (x + 0) (y + w) w q4 (* top left     *)
;;

let () =
  try
    ()
    ; G.open_graph " 256x256"
    ; print_endline "Press any key to exit"
    ; while true do
        ()
        ; if G.key_pressed () then raise Exit
        ; draw 0 0 256 (checker_board 3) (* 8x8 checkerboard for 2³ = 8 *)
      done
  with
  | Exit -> G.close_graph ()
;;
