(*
Two compilation modes are available:

1. Bytecode complilation with `ocamlc`, independant from the CPU architecture.
2. Native code complilation with `ocamlopt`, faster but also bigger.

Prior to dune, orchestrating build steps was done with either `make` or `ocamlbuild`.

======

ocamlc ./hello.ml0
./a.out

ocamlc -o hello-bc.exe ./hello.ml
./hello-bc

ocamlopt -o hello-native.exe ./hello.ml

======

$ file hello-*
hello-bc:     a /home/benjamin/.opam/4.14.0/bin/ocamlrun script executable (binary data)
hello-native: ELF 64-bit LSB shared object, x86-64, version 1 (SYSV), dynamically linked, interpreter /lib64/ld-linux-x86-64.so.2, BuildID[sha1]=c3648ee300db22a85522227ae797d4d0606c131f, for GNU/Linux 3.2.0, with debug_info, not stripped

======

$ du -sh hello-*
24K     hello-bc
1.4M    hello-native

======

$ time ./hello-bc && echo "---" && time ./hello-native
Hello, World!

real    0m0.004s
user    0m0.001s
sys     0m0.003s
---
Hello, World!

real    0m0.002s
user    0m0.000s
sys     0m0.002s

*)

let () = print_string "Hello, World!\n"