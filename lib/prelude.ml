open Printf

let id x = x
let ($) f g = fun x -> f (g x)
let fail fmt = ksprintf failwith fmt
