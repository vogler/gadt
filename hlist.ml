(* heterogeneous lists *)
module Option = struct
  let nil = None
  let cons x xs = Some (x, xs)

  let t1 = cons 1 (cons "a" nil)

  let head = function Some (x, xs) -> Some x | None -> None
  let tail = function Some (x, xs) -> xs | None -> None

  (* polymorphic recursion doesn't work since option's type is not existential *)
  (* let rec length : type a b. (a*b) option -> int = function *)
  (*   | Some (_, xs) -> 1 + length xs *)
  (*   | None -> 0 *)
end

module GADT = struct
  type _ t = [] : unit t | (::) : 'a * 'b t -> ('a * 'b) t

  let t1 = [1; "a"]

  let head : type a. (a*_) t -> a = function x::_ -> x
  let tail : type a. (_*a) t -> a t = function _::x -> x

  (* how can I have one function that handles both cases? *)
  let head_opt : unit t -> 'a option = function [] -> None
  let head_opt : type a. (a*_) t -> a option = function x::_ -> Some x

  let rec length : type a. a t -> int = function
    | [] -> 0
    | _::xs -> 1 + length xs

  (* let rec map : type a. (a -> 'b) -> a t -> 'b list = fun f -> function *)
  (*   | [] -> List.[] *)
  (*   | x::xs -> f x List.(::) map xs *)
end

module GADT2 = struct
  type _ t = [] : ('a * unit) t | (::) : 'a * 'b t -> ('a * 'b) t

  let t1 = [1; "a"]

  let head_opt : type a. (a*_) t -> a option = function x::_ -> Some x | [] -> None
end

module GADT3 = struct
  (* https://discuss.ocaml.org/t/in-pattern-matching/2676/2 *)
  (* this has the nicest most readable type for t1 (the one in GADT2 has parens around every tuple). *)
  type empty = |
  type _ hlist = []: empty hlist | (::): 'a * 'b hlist -> ('a->'b) hlist

  let t1 = [1; "a"]

  let hd (type a b): (a -> b) hlist -> a = fun (a :: _) -> a

  let x = hd t1
end
