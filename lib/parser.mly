(*
  https://github.com/Qiskit/openqasm/blob/master/spec/qasm2.rst#open-qasm-grammar
*)

%{
open Syntax
%}

%token <int> INT
%token <float> REAL
%token <string> ID
%token <Syntax.unop> UNOP

%token OPENQASM QREG CREG GATE OPAQUE BARRIER MEASURE RESET IF PI
%token EOI
%token LBRACKET RBRACKET LPAREN RPAREN LCURLY RCURLY
%token COMMA SEMICOLON ARROW EQ PLUS MINUS MUL DIV POW

%left PLUS MINUS
%left MUL DIV
%nonassoc POW

%start <Syntax.program> program

%%

let parentheses(x) == delimited(LPAREN,x,RPAREN)

let program := OPENQASM; ~ = REAL; SEMICOLON; ~ = nonempty_list(terminated(statement,SEMICOLON) | gatedecl); EOI; <>

let statement :=
  | QREG; ~ = ID; LBRACKET; ~ = INT; RBRACKET; <QReg>
  | CREG; ~ = ID; LBRACKET; ~ = INT; RBRACKET; <CReg>
  | OPAQUE; ~ = gate(ID,idlist); <Opaque>
  | ~ = qop; <Q>
  | BARRIER; ~ = anylist; <Barrier>
  | IF; ~ = parentheses(separated_pair(ID,EQ,INT)); ~ = qop; <If>

let qop :=
  | ~ = gate(exp,anylist); <Op>
  | MEASURE; a = arg; ARROW; b = arg; { Measure (a,b) }
  | RESET; ~ = arg; <Reset>

let gate(param,args) := id = ID; params = parentheses(separated_list(COMMA, param))?; ~ = args; { { id; params; args } }

let arg := ~ = ID; < Reg > | ~ = indexed; <Index>
let binop == PLUS; {Add} | MINUS; {Sub} | MUL; {Mul} | DIV; {Div} | POW; {Pow}
let exp :=
  | ~ = REAL; <Real>
  | ~ = INT; <Int>
  | PI; { Pi }
  | ~ = ID; <R>
  | ~ = midrule(MINUS; {Neg}); ~ = exp; <Unaryop> %prec MUL
  | parentheses(exp)
  | a = exp; op = binop; b = exp; { Binop(a,op,b) }
  | ~ = UNOP; ~ = parentheses(exp); <Unaryop>

(* let explist := separated_nonempty_list(COMMA, exp) *)
let idlist := separated_nonempty_list(COMMA, ID)
let indexed := ~ = ID; LBRACKET; ~ = INT; RBRACKET; <>
let anylist := separated_nonempty_list(COMMA, arg)

let gop := ~ = gate(exp,anylist); <GOp>
let gbarrier := BARRIER; ~ = idlist; <GBarrier>
let gatedecl := GATE; ~ = gate(ID,idlist); LCURLY; ~ = list(terminated(midrule(gop|gbarrier),SEMICOLON)); RCURLY; <Gate>
