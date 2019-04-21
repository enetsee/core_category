include Bind_intf

(* -- Helpers --------------------------------------------------------------- *)
module type Basic_general = sig
  type ('a, 'i, 'j, 'd, 'e) t

  val bind :
       ('a, 'i, 'j, 'd, 'e) t
    -> f:('a -> ('b, 'i, 'j, 'd, 'e) t)
    -> ('b, 'i, 'j, 'd, 'e) t

  val join :
    [ `Define_using_bind
    | `Custom of
      (('a, 'i, 'j, 'd, 'e) t, 'i, 'j, 'd, 'e) t -> ('a, 'i, 'j, 'd, 'e) t
    ]
end

module Make_general (X : Basic_general) = struct
  let bind = X.bind

  let join_via_bind m = bind m ~f:(fun x -> x)

  let join =
    match X.join with `Define_using_bind -> join_via_bind | `Custom f -> f


  module Bind_infix = struct
    let ( >>= ) m f = bind m ~f

    let ( =<< ) f m = bind m ~f

    let ( >> ) m n = bind m ~f:(fun _ -> n)
  end

  include Bind_infix
end

(* -- Functors -------------------------------------------------------------- *)

module Make (X : Basic) : S with type 'a t := 'a X.t = struct
  include Make_general (struct
    type ('a, 'i, 'j, 'd, 'e) t = 'a X.t

    include (X : Basic with type 'a t := 'a X.t)
  end)

  include Apply.Make (X)
end
