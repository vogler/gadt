#require "batteries";;
open Batteries

module A = struct
  (* container * content * sig * sig_v * sig_a *)
  type _ t = V : 'a ref -> ('a ref*'a * 'b * 'b * 'c) t | A : 'a array -> ('a array*'a * 'c * 'b * 'c) t;;
  let get : type k c f. (k*c*f*c*(int->c)) t -> f = function V x -> !x | A a -> fun i -> a.(i)
  let set : type k c f. (k*c*f*(c->unit)*(int*c->unit)) t -> f = function V x -> fun y -> x := y | A a -> fun (i,x) -> ();;
  let incr : type k c f. (k*c*f*unit*(int->unit)) t -> f = function V x -> () | A a -> fun i -> ();;

  get (V (ref 1));;
  get (A (Array.make 3 1));;
  set (V (ref 1));;
  set (A (Array.make 3 1));;

  (* needed to add first type variable to make matches exhaustive: *)
  let f (V x) = 1;;

  (* the problem is that if a constructor has a weakly poly. arg. like ref or array, then all the other type variables become weak too :( *)
  let v = V (ref 1);;
  get v;;
  (* set v 2;; (* this will fail *) *)
end

module B = struct
  (* container, content, sig, sig_v, sig_a *)
  type (_,_,_,_,_) t = V : 'a -> ('a ref, 'a, 'b, 'b, 'c) t | A : 'a list -> ('a array, 'a, 'c, 'b, 'c) t;;
  let get : type k c f. (k,c,f,c,(int->c)) t -> f = function V x -> x | A a -> fun i -> List.at a i
  let set : type k c f. (k,c,f,(c->unit),(int*c->unit)) t -> f = function V x -> fun x -> () | A a -> fun (i,x) -> ();;
  let incr : type k c f. (k,c,f,unit,(int->unit)) t -> f = function V x -> () | A a -> fun i -> ();;

  (* needed to add first type variable to make matches exhaustive: *)
  let f (V x) = 1;;

  let v = V 1;;
  get v;;
  set v 2;;
  let a = A ([1;2;3]);;
  get a 2;;
  set a (2,2);;
end
