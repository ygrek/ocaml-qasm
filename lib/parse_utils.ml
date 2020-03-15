(* Simple wrapper tying together parser and lexer *)

module type Parser_type =
sig
  type token
  type result
  val input : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> result
  val rule : Lexing.lexbuf -> token
  val tail : string -> Lexing.lexbuf -> string
end

exception Error of exn * (int * int * string * string)

let truncate s = if String.length s > 32 then String.sub s 0 32 ^ "..." else s

let () = Printexc.register_printer begin function
  | Error (exn, (line,cnum,tok,tail)) ->
    Some (Printf.sprintf "Parse.Error at %d:%d, token %S followed by %S : %s" line cnum tok (truncate tail) (Printexc.to_string exn))
  | _ -> None
end

module Make(T : Parser_type) =
struct
  let parse_buf_exn lexbuf =
    try
      T.input T.rule lexbuf
    with exn ->
      begin
        let curr = lexbuf.Lexing.lex_curr_p in
        let line = curr.Lexing.pos_lnum in
        let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
        let tok = Lexing.lexeme lexbuf in
        let tail = T.tail "" lexbuf in
				raise (Error (exn,(line,cnum,tok,tail)))
      end

  let stdin () = parse_buf_exn (Lexing.from_channel stdin)
  let string str = parse_buf_exn (Lexing.from_string str)

end
