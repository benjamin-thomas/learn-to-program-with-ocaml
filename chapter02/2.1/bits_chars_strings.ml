(*
  echo bits_chars_strings.ml | entr -cr bash -c 'ocamlc -o ./bits_chars_strings.exe ./bits_chars_strings.ml && ./bits_chars_strings.exe'

`land`: means bitwise "logical and"

Bitwise "logical and" definition:
    0101 (decimal 5)
AND 0011 (decimal 3)
  = 0001 (decimal 1)

`lsl`: `n` lsl `m` shifts `n` to the left by m bits

Examples:

  1 lsl 3 =       8
      0b1 => 0b1000

  8 lsl 1 =   0b10000
  0b10000 => 0b100000
*)

let () =
  assert (0b101010 = 42);

  assert (42 land (1 lsl 0) = 0);
  assert (0b101010 land (1 lsl 0) = 0b0);
  Printf.printf "0b101010: bit 0 from the right has value 0\n";
  assert (42 land (1 lsl 1) = 2);
  assert (0b101010 land (1 lsl 1) = 0b10);
  Printf.printf "0b101010: bit 1 from the right has value 2\n";
  assert (42 land (1 lsl 2) = 0);
  assert (0b101010 land (1 lsl 2) = 0b000);
  Printf.printf "0b101010: bit 2 from the right has value 0\n";
  assert (42 land (1 lsl 3) = 8);
  assert (0b101010 land (1 lsl 3) = 0b1000);
  Printf.printf "0b101010: bit 3 from the right has value 8\n";

  (* lsl usage *)
  assert (1 lsl 0 = 0b1);
  assert (1 lsl 1 = 0b10);
  assert (1 lsl 2 = 0b100);
  assert (1 lsl 3 = 0b1000);
  assert (1 lsl 3 = 8);
  assert (8 lsl 1 = 0b10000);
  assert (8 lsl 1 = 16);

  assert ('a' = '\097');
  assert ('a' = '\x61');
  assert (Char.code 'a' = 97);
  assert (Char.chr (97 + 1) = 'b');
  assert ("abc" = "\097bc");
  assert ('a' = "abc".[0]);
  assert ('b' = "abc".[1]);

  assert ("abcdef" = "abc" ^ "def");
  assert (3 = String.length "abc");

  prerr_endline "This prints to STDERR";
  print_endline "This prints to STDOUT";

  let _ =
    print_newline ();
    print_string "Other means of printing...";
    print_newline ();
    print_newline ();
    let _ = List.map print_char [ 'A'; 'B'; 'C'; '\n' ] in
    let _ = List.map prerr_char [ 'D'; 'E'; 'F'; '\n' ] in
    print_float 42.;
    print_newline ();
    Printf.printf "STDOUT again...\n";
    Printf.eprintf "STDERR again...\n"
  in

  flush_all ();
  print_endline "Finished!"
