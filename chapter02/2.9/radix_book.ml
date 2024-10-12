(*
   rg --files | entr -c bash -c 'dune exec --no-print-directory --display=quiet chapter02/2.9/OCaml/radix_book.exe 16 < <(echo -e "ABC\nDEF\nFF")'
*)

open Printf

let list_of_string str =
  let digits = ref [] in
  for i = 0 to String.length str - 1 do
    digits := str.[i] :: !digits
  done;
  !digits
;;

let digit_of_char c =
  match c with
  | '0' .. '9' -> Char.code c - 48
  | 'A' .. 'Z' -> Char.code c - 55
  | _ -> invalid_arg @@ sprintf "digit_of_char: %c" c
;;

let base = int_of_string Sys.argv.(1)
let check_digit n = if n < 0 || n >= base then invalid_arg @@ sprintf "Bad digit: %d" n

(*{|
       For ABC₁₆ = 2748₁₀
       12 11 10 -> 2748

       Prelude> 12+16*(11+16*(10+16*(0)))
       2748

       Prelude> 12+0 + 11*16^1 + 10*16^2
       2748

       Prelude Data.Char> (digitToInt 'A')*16^2 + (digitToInt 'B')*16^1 + (digitToInt 'C')*16^0
       2748

       Prelude Data.Char> digitToInt 'C' + 16*(digitToInt 'B' + 16*digitToInt 'A')
       2748

       Prelude Data.Char> foldl (\acc n -> 16 * acc + digitToInt n) 0 ['A', 'B', 'C']
       2748

       Prelude Data.Char> foldr (\n acc -> 16 * acc + digitToInt n) 0 (reverse ['A', 'B', 'C'])
       2748

       "Naturally", I'd tend to think in those terms:
       Prelude Data.Char> snd $ foldl (\(i, acc) n -> (i+1, acc + (digitToInt n * 16^i))) (1, digitToInt 'C') ['B', 'A']
       2748

       However, computing n powers has an exponential cost. Which I can reduce, like so:
       Prelude Data.Char> (\(_,_,x) -> x) $ foldl (\(i, j, acc) n -> (i+1, j*16, acc + (digitToInt n *j))) (1, 16, digitToInt 'C') ['B', 'A']
       2748

       However, that's still computationally more expensive than Horner's method, which states that:

       > A×16² + B×16¹ + C×16⁰

       Is equivalent to:

       > C + 16×(B + 16×(A + 16×0))

       So basically:

       Prelude Data.Char> foldl (\acc n -> digitToInt n + 16*acc ) 0 ['A' ,'B', 'C']
       2748

       ---

       Ruby:

       [1] pry(main)> ['A', 'B', 'C'].map { _1.to_i(16) }.reduce { |acc,n| n + 16*acc }
       => 2748
       [2] pry(main)> ['A', 'B', 'C'].reduce(0) { |acc,n| n.to_i(16) + 16*acc }
       => 2748

       Ruby, imperative style:

       acc = 0
       for str in ['A', 'B', 'C']
         acc = str.to_i(16) + 16*acc
       end
       p acc

       ---

       F#

       > ['A';'B';'C'] |> List.map (fun (c: char) -> System.Convert.ToInt32(c)-55) |> List.reduce(fun acc n -> n + 16*acc);;
       val it: int = 2748

       > ['A';'B';'C'] |> List.fold(fun acc n -> System.Convert.ToInt32(n)-55 + 16*acc) 0;;
       ival it: int = 2748
  |}*)
let run () =
  try
    while true do
      let str = read_line () in
      let chars = list_of_string str in
      let digits = List.map digit_of_char chars in
      List.iter check_digit digits;
      List.iter (printf "%d ") digits;
      let v = List.fold_right (fun n acc -> (base * acc) + n) digits 0 in
      printf "\n -> %d\n\n" v
    done
  with
  | End_of_file -> ()
;;

run ()
