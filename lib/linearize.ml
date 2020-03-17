open Prelude
open Syntax

let program l =
  let h = Hashtbl.create 10 in
  l |> List.iter begin function
    | Gate (g,ops) ->
      begin match Hashtbl.find h g.id with
      | _ -> fail "symbol %s already defined" g.id
      | exception _ -> Hashtbl.add h g.id (g,ops)
      end
    | _ -> ()
  end;
  let rec resolve ({ id; params=_; args } as op) =
    match Hashtbl.find h id with
    | exception _ -> [Q (Op op)]
    | ({ args = vars; _ }, ops) ->
      if List.length vars <> List.length args then fail "bad application %s : args list mismatch" id;
      let vmap = List.combine vars args in
      let vmap x = match List.assoc x vmap with y -> y | exception _ -> Reg x in
      let vmap = function Reg x -> vmap x | Index (r,n) -> fail "cannot substitute %s[%d]" r n in
      ops |> List.map begin function
        | GOp op -> resolve { op with args = List.map vmap op.args }
        | GBarrier l -> [Barrier (List.map (fun id -> vmap (Reg id)) l)]
      end |> List.flatten
  in
  l |> List.map begin function
    | Gate _ -> []
    | Q (Op op) -> resolve op
    | stmt -> [stmt]
  end
  |> List.flatten
