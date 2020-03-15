
let pr fmt = Printf.ksprintf print_endline fmt

let () =
  let (v,l) = Qasm.Parse.stdin () in
  pr "version %f" v;
  pr "%d statements" (List.length l);
  ()
