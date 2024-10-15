(* Suppose we want to represent ints between 0 and 30 *)
module type INT31 = sig
  type t

  val create : int -> t
  val value : t -> int
end

module Int31 : INT31 = struct
  type t = int

  let check x = if x < 0 || x > 30 then invalid_arg "Int31.create"

  let create x =
    check x;
    x
  ;;

  let value x = x
end

open Printf

let () =
  (* One more and it would fail at runtime *)
  let x = Int31.create 30 in
  printf "  x = %d\n" (Int31.value x);
  printf "x+1 = %d\n" (Int31.value x + 1);
  ()
;;

(*
   TODO: as in `Int31`, we could implement garanties about the type at construction time
*)
module type POLAR = sig
  type t

  val create : float -> float -> t
  val rho : t -> float
  val theta : t -> float
end

module Polar : POLAR = struct
  type t =
    { rho : float
    ; theta : float
    }

  let create r t = { rho = r; theta = t }

  (* Because we make `t` abstract, we need to provide functions to allow access
     `rho` and `theta` from outside the module *)
  let rho x = x.rho
  let theta x = x.theta
end

module type POLAR2 = sig
  type t = private
    { rho : float
    ; theta : float
    }

  val create : float -> float -> t
end

module Polar2 : POLAR2 = struct
  type t =
    { rho : float
    ; theta : float
    }

  let create r t = { rho = r; theta = t }
end

(* In both cases, it is not possible to construct the type manually, we _have_ to go though `create` *)
let () =
  (* Advantage: `t` is abstract. Disadvantage: we have to create accessor functions. *)
  let a = Polar.create 1. 2. in
  printf "{rho = %f; theta = %f}\n" (Polar.rho a) (Polar.theta a);
  (* Advantage: no accessor functions are necessary. Disadvantage: we must define `t` twice. *)
  let b = Polar2.create 3. 4. in
  printf "{rho = %f; theta = %f}\n" b.rho b.theta;
  ()
;;
