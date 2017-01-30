type z
type +'n s = T
type _ t = Z : z t | S : 'a t -> 'a s t;;
Z;;
S (S Z);;
(* let rec from = function *)
(*   | 0 -> Z *)
(*   | 1 -> S Z *)
let _0 = Z
let _1 = S _0
let _2 = S _1
let _3 = S _2
