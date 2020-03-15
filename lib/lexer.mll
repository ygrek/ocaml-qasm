
{

open Lexing
open Parser

let error _ callerID =
  prerr_endline (Printf.sprintf "Lexer error : %s" callerID);
  raise Parsing.Parse_error

let pos lexbuf = (lexeme_start lexbuf, lexeme_end lexbuf)
let advance_line_pos pos = { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum; }
let advance_line lexbuf = lexbuf.lex_curr_p <- advance_line_pos lexbuf.lex_curr_p

}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let ident = ['a'-'z'] (alpha | digit | '_')*
let nninteger = (['1'-'9']+ digit* | '0')
let wsp = [' ' '\r' '\t']
let real = (digit+ '.' digit* | digit* '.' digit+ ) (['e' 'E']['-' '+']?digit+)?

(* extract tail of the input *)
rule tail acc = parse
  | eof { acc }
  | _* as str { tail (acc ^ str) lexbuf }

and token = parse
  | wsp   { token lexbuf }
  (* update line number *)
  | '\n'  { advance_line lexbuf; token lexbuf }

  | ','   { COMMA }
  | ';'   { SEMICOLON }
  | "->"  { ARROW }
  | "=="  { EQ }

  | '*' { MUL }
  | '/' { DIV}
  | '+' { PLUS }
  | '-' { MINUS }
  | '^' { POW }

  | '['   { LBRACKET }
  | ']'   { RBRACKET }
  | '('		{ LPAREN }
  | ')'		{ RPAREN }
  | '{'   { LCURLY }
  | '}'   { RCURLY }

  | "//" { ignore (comment "" lexbuf); token lexbuf }

  | "OPENQASM" { OPENQASM }
  | "include" wsp '"' [^ '"']+ '"' ';' { token lexbuf } (* TODO *)

  | "qreg" { QREG }
  | "creg" { CREG }
  | "gate" { GATE }
  | "barrier" { BARRIER }
  | "measure" { MEASURE }
  | "reset" { RESET }
  | "if" { IF }
  | "pi" { PI }
  | "sin" { UNOP Sin }
  | "cos" { UNOP Cos }
  | "tan" { UNOP Tan }
  | "exp" { UNOP Exp }
  | "ln" { UNOP Ln }
  | "sqrt" { UNOP Sqrt }

  | ident as str { ID str }
  | nninteger as str { INT (int_of_string str) }
  | real as str { REAL (float_of_string str) }

  | eof	{ EOI }

  | _	{ error lexbuf "token" }

and comment acc = parse
  | '\n'	{ advance_line lexbuf; acc }
  | eof	        { acc }
  | [^'\n']+    { let s = lexeme lexbuf in comment (acc ^ s) lexbuf; }
  | _		{ error lexbuf "comment"; }
