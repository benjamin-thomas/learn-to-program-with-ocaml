(*
   dune exec --no-print-directory --display=quiet bin/turtle.exe -w
*)

(* `t` is an "abstract" type, it represent the type chosen by the instanciated module
 * Think "Interface" in Java terms
 *)
module type ANGLE = sig
  type t

  val of_degrees : float -> t
  val add : t -> t -> t
  val cos : t -> float
  val sin : t -> float
end

(* The `Turtle` module is paramaterized by the module of type `ANGLE`, AKA a `Functor *)
module Turtle (A : ANGLE) = struct
  let is_drawing = ref true
  let pen_down () = is_drawing := true
  let pen_up () = is_drawing := false
  let angle = ref (A.of_degrees 0.)
  let rotate_left d = angle := A.add !angle (A.of_degrees d)
  let rotate_right d = rotate_left (-.d)

  module G = Graphics

  let tx = ref 400
  let ty = ref 300

  let () =
    G.open_graph " 800x600";
    G.moveto !tx !ty;
    G.set_line_width 2
  ;;

  let advance d =
    tx := !tx + truncate (d *. A.cos !angle);
    ty := !ty + truncate (d *. A.sin !angle);
    if !is_drawing then G.lineto !tx !ty else G.moveto !tx !ty
  ;;
end

(* To use our `Turtle` module, we first need to define a module of `ANGLE` interface
 * Think `Impl` (of an interface) in Java terms
 *)
module Angle : ANGLE = struct
  type t = float

  let pi_over_180 = atan 1. /. 45.
  let of_degrees d = d *. pi_over_180
  let add = ( +. )
  let cos = Stdlib.cos
  let sin = Stdlib.sin
end

(* Now we can now create a `T` module by applying the `Turtle` functor to the `Angle` module *)
module T = Turtle (Angle)

(* And we can finally use `T` to draw *)
let square d =
  for k = 1 to 4 do
    T.advance d;
    T.rotate_left 90.
  done
;;

let squares d a =
  for k = 1 to truncate (360. /. a) do
    square d;
    T.rotate_left a
  done
;;

let () =
  squares 100. 20.;
  read_line () |> ignore;
  ()
;;
