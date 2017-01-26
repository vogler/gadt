module A = struct

  module type IO = sig
    type 'a t
    type rw (* read write *)
    type r (* read *)

    val fopen : string -> rw t
    val fopen_ro : string -> r t
    val fread : 'a t -> string (* this could be called on closed! *)
    val fwrite : rw t -> unit
    val fclose : 'a t -> unit t
  end

  module IO : IO = struct
    type 'a t
    type rw
    type r
    let fopen x = failwith ""
    let fopen_ro x = failwith ""
    let fread x = failwith ""
    let fwrite x = failwith ""
    let fclose x = failwith ""
  end
end

module B = struct
  module type IO = sig
    type 'a t

    val fopen : string -> [`R | `W] t
    val fopen_ro : string -> [`R] t
    val fread : [>`R] t -> string (* this could be called on closed! *)
    val fwrite : [>`W] t -> unit
    val fclose : [`R | `W] t -> unit t
  end

  module IO : IO = struct
    type 'a t
    type _ perm = RO : [`R ] perm | RW : [`R | `W] perm
    let fopen x = failwith ""
    let fopen_ro x = failwith ""
    let fread x = failwith ""
    let fwrite x = failwith ""
    let fclose x = failwith ""
  end

  let () = let open IO in
    let w = fopen "a" in
    let r = fopen_ro "a" in
    let c = fclose w in
    (* ok *)
    let _ = fread r in
    let _ = fread w in
    fwrite w;
    (* compile errors: *)
    let d = fclose c in (* double close *)
    let _ = fread c in (* read closed *)
    fwrite r; (* write readonly *)
end
