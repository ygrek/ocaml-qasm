
let pr fmt = Printf.ksprintf print_endline fmt

let () =
  let l = Qasm.Parse.stdin () in
  pr "%d statements" (List.length l);
  ()
