open Qasm

let pr fmt = Printf.ksprintf print_endline fmt

let () =
  let l = Parse.stdin () in
  pr "%d statements" @@ List.length l;
  pr "%s" @@ Syntax.show_program l;
  pr "";
  Print.program l;
  ()
