(*
   dune exec ./approx_pi3.exe
*)

open Printf

let leibniz_term n =
  if n mod 2 == 0 then
    1.0 /. float_of_int ((2 * n) + 1)
  else
    -1.0 /. float_of_int ((2 * n) + 1)
;;

let approx_pi n =
  let lst = List.init (n + 1) Fun.id |> List.map leibniz_term in
  4. *. List.fold_left (fun total curr -> total +. curr) 0.0 lst
;;

let () =
  ()
  ; print_newline ()
  ; printf "Approx PI (0): %f\n" @@ approx_pi 0
  ; printf "Approx PI (1): %f\n" @@ approx_pi 1
  ; printf "Approx PI (2): %f\n" @@ approx_pi 2
  ; printf "Approx PI (3): %f\n" @@ approx_pi 3
  ; printf "Approx PI (4): %f\n" @@ approx_pi 4
  ; printf "Approx PI (5): %f\n" @@ approx_pi 5
  ; printf "Approx PI(10): %f\n" @@ approx_pi 10
  ; printf "Approx PI(20): %f\n" @@ approx_pi 20
  ; printf "Approx PI(40): %f\n" @@ approx_pi 40
  ; printf "Approx PI(80): %f\n" @@ approx_pi 80
;;
