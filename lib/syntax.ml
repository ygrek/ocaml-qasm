
type id = string

type binop = Add | Sub | Mul | Div | Pow
type unop = Sin | Cos | Tan | Exp | Ln | Sqrt | Neg
type exp =
| Int of int
| Real of float
| Pi
| R of id
| Binop of exp * binop * exp
| Unaryop of unop * exp

type arg = Reg of id | Index of (id * int)

type ('param, 'arg) gate = { id : id; params : 'param list option; args : 'arg list; }

type op = (exp, arg) gate

type gate_op =
| GOp of op
| GBarrier of id list

type qop =
| Op of op
| Measure of arg * arg
| Reset of arg

type statement =
| Gate of ((id, id) gate * gate_op list)
| Opaque of (id, id) gate
| CReg of id * int
| QReg of id * int
| Q of qop
| Barrier of arg list
| If of (id * int) * qop

type program = float * statement list
