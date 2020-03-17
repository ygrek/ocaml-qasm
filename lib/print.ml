open Printf
open Syntax
open Prelude

let print s = print_endline (s ^ ";")
let pr fmt = ksprintf print fmt
let indent s = "  " ^ s

let catmap show l = l |> List.map show |> String.concat ","
let gate param arg { id; params; args } =
  let params =
    match params with
    | None -> ""
    | Some l -> sprintf "(%s)" (catmap param l)
  in
  sprintf "%s%s %s" id params (catmap arg args)

let arg = function Reg s -> s | Index (s,n) -> sprintf "%s[%d]" s n
let exp = function _ -> "exp"
let op = gate exp arg
let barrier show l = sprintf "barrier %s" (catmap show l)
let qop = function
  | Op x -> op x
  | Measure (a, b) -> sprintf "measure %s -> %s" (arg a) (arg b)
  | Reset r -> sprintf "reset %s" (arg r)

let gop = function
  | GOp x -> op x
  | GBarrier l -> barrier id l

let program l =
  pr "OPENQASM 2.0";
  pr "include \"qelib1.inc\"";
  l |> List.iter begin function
    | CReg (s,n) -> pr "creg %s[%d]" s n
    | QReg (s,n) -> pr "qreg %s[%d]" s n
    | Gate (g, ops) ->
      print_endline @@ sprintf "gate %s {" (gate id id g);
      List.iter (print $ indent $ gop) ops;
      print_endline "}"
    | Opaque g -> pr "opaque %s" (gate id id g)
    | Barrier l -> print @@ barrier arg l
    | Q x -> print @@ qop x
    | If ((id,n),x) -> pr "if (%s == %d) %s" id n (qop x)
  end
