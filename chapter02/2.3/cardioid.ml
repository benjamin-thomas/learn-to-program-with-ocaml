open Graphics

(*
   ocamlfind ocamlc -linkpkg -package graphics unix.cma -o ./cardioid.exe ./cardioid.ml && ./cardioid.exe
*)

(*
   Compilation instructions.

   Contrary to the Sys, Arg or Printf modules, Graphics doesn't belong tot the standard library.

   For this reason, we must *explicitly* tell the compiler about it, by generating a `graphics.cma` file this way:

   ocamlc -o ./cardioid.exe graphics.cma ./cardioid.ml
   ocamlopt -o ./cardioid.exe graphics.cmxa ./cardioid.ml

   ocaml graphics.cma # REPL
   ocaml ~/.opam/4.14.0/lib/graphics/graphics.cma # Only the *.cma file can be loaded, not the *.cmxa

   Use an *.cmxa extension to compile to a native binary.

   NOTE: the order of the files matter, hence `graphics.cma` **MUST** be specified before `cardioid.ml`.

   ocamlfind ocamlc -linkpkg -package graphics -o ./cardioid.exe ./cardioid.ml
   ocamlfind ocamlopt -linkpkg -package graphics -o ./cardioid.exe ./cardioid.ml
   rm *.exe *.cmi *.cmx *.o *.cmo

   $ ocamlfind query graphics
   ~/.opam/4.14.0/lib/graphics

   $ file ~/.opam/4.14.0/lib/graphics/graphics.cm*a
   ~/.opam/4.14.0/lib/graphics/graphics.cma:  OCaml library file (.cma) (Version 031)
   ~/.opam/4.14.0/lib/graphics/graphics.cmxa: OCaml native library file (.cmxa) (Version 031)

   ls $(opam var lib)

   So the `graphics` module **used to be packaged** with Ocaml.
   ls -l $(ocamlc -where)/g*

   Which explains why including the unix module works, but not graphics!

   ocamlc -o hello.exe unix.cma ./hello.ml # All good

   I can also load the unix module from any directory.

   $ ocaml unix.cma

   So to sum up, I'll have to mostly use `ocamlfind` from now on (unless dune appears to be more appropriate)

   dune exec ./cardioid.exe
*)

(* https://discuss.ocaml.org/t/new-pi-constant/2199 *)
let all_eq_pi =
  [ acos (-1.0)
  ; 4.0 *. atan 1.0
  ; 2.0 *. asin 1.0
  ; 2.0 *. acos 0.0
  ; 4.0 *. atan 1.0
  ; (2.0 *. Complex.(arg i))
  ; (2.0 *. Complex.((log i).im))
  ]
[@@warning "-32"]
;;

let () =
  (* Open graphics window. Space prefix is necessary. Origin is at the bottom left. *)
  open_graph " 300x200"
;;

let () =
  let radius_length = 50.0 in
  (* Set current point *)
  moveto 200 150
  ; Unix.sleepf 0.5
  ; for i = 0 to 200 do
      (*
         Change the theta angle between 0 and 2PI
         Note that: 4 * atan 1 = PI

         In other words, between:

         atan(1) * (0 / 25) = 0
         atan(1) *       0  = 0

         And

         atan(1) * (200 / 25) = approx. 6.2831853
         atan(1) *         8  = approx. 6.2831853
      *)
      let theta = atan 1.0 *. float i /. 25.0 in
      let radius = radius_length *. (1.0 -. sin theta) in
      (* Trace segment from current point to coordinate *)
      lineto (150 + truncate (radius *. cos theta)) (150 + truncate (radius *. sin theta))
      ; Unix.sleepf 0.008
    done
  ; (* Wait for a keypress to prevent exit after loop end. We ignore the returned value. *)
    (* ignore (read_key ()) *)
    Unix.sleepf 0.5
;;
