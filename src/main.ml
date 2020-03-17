open Qasm

let pr fmt = Printf.ksprintf print_endline fmt

let () =
  let l = Parse.stdin () in
  match List.tl @@ Array.to_list Sys.argv with
  | ["show"] -> Print.program l
  | ["debug"] ->
    pr "%d statements" @@ List.length l;
    pr "%s" @@ Syntax.show_program l;
  | ["linearize"] ->
    Print.program @@ Linearize.program l
  | _ -> failwith "nah"
