(* http://www.kb.ecei.tohoku.ac.jp/ml2008/slides/lindley.pdf *)
(* no GATDs needed, just difference types *)

type z (* zero *)
type +'n s (* successor *)

module Nat : sig
  type +'i t
  val zero : ('m*'m) t
  val succ : ('m*'n) t -> ('m*'n s) t
  val add  : ('m*'n) t -> ('l*'m) t -> ('l*'n) t
  val to_int : 'i t -> int
end = struct
  type 'i t = int
  let zero = 0
  let succ n = n+1
  let add n m = n+m
  let to_int n = n
end

module NList : sig
  type (+'length, +'elem_type) t
  val nil : ('m*'m, 'a) t
  val cons : 'a -> ('m*'n, 'a) t -> ('m*'n s, 'a) t
  val append : ('m*'n, 'a) t -> ('l*'m, 'a) t -> ('l*'n, 'a) t
  val to_list : ('i, 'a) t -> 'a list
end = struct
  type ('i, 'a) t = 'a list
  let nil = []
  let cons x xs = x :: xs
  let append xs ys = xs @ ys
  let to_list xs = xs
end
