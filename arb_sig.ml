type _ t = V : 'a -> ('a ref*'a * 'b * 'b * 'c) t | A : 'a array -> ('a array*'a * 'c * 'b * 'c) t;;
let get : type k c f. (k*c*f*c*(int->c)) t -> f = function V x -> x | A a -> fun i -> a.(i)
let set : type k c f. (k*c*f*(c->unit)*(int*c->unit)) t -> f = function V x -> fun x -> () | A a -> fun (i,x) -> ();;

get (V 1);;
get (A (Array.make 3 1));;
set (V 1);;
set (A (Array.make 3 1));;

let incr : type k c f. (k*c*f*unit*(int->unit)) t -> f = function V x -> () | A a -> fun i -> ();;

(* needed to add first type variable to make matches exhaustive: *)
let f (V x) = 1;;
