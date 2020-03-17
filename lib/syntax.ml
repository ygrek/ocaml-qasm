
type id = string [@@deriving show { with_path = false }]

type binop = Add | Sub | Mul | Div | Pow [@@deriving show { with_path = false }]
type unop = Sin | Cos | Tan | Exp | Ln | Sqrt | Neg [@@deriving show { with_path = false }]
type exp =
| Int of int
| Real of float
| Pi
| R of id
| Binop of exp * binop * exp
| Unaryop of unop * exp
[@@deriving show { with_path = false }]

type arg = Reg of id | Index of (id * int) [@@deriving show { with_path = false }]

type ('param, 'arg) gate = { id : id; params : 'param list option; args : 'arg list; } [@@deriving show { with_path = false }]

type op = (exp, arg) gate
[@@deriving show { with_path = false }]

type gate_op =
| GOp of op
| GBarrier of id list
[@@deriving show { with_path = false }]

type qop =
| Op of op
| Measure of arg * arg
| Reset of arg
[@@deriving show { with_path = false }]

type statement =
| Gate of ((id, id) gate * gate_op list)
| Opaque of (id, id) gate
| CReg of id * int
| QReg of id * int
| Q of qop
| Barrier of arg list
| If of (id * int) * qop
[@@deriving show { with_path = false }]

type program = statement list [@@deriving show { with_path = false }]
