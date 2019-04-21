include Apply_intf

module type Basic_general = sig
  type ('a, 'i, 'j, 'd, 'e) t

  val apply :
       ('a, 'i, 'j, 'd, 'e) t
    -> f:('a -> 'b, 'i, 'j, 'd, 'e) t
    -> ('b, 'i, 'j, 'd, 'e) t

  val map : ('a, 'i, 'j, 'd, 'e) t -> f:('a -> 'b) -> ('b, 'i, 'j, 'd, 'e) t
end

module Make_general (F : Basic_general) = struct
  let apply = F.apply

  let const x _ = x

  let identity x = x

  let liftA2 f a b = apply b ~f:(F.map a ~f)

  let apply_const a b = apply b ~f:(F.map ~f:(const identity) a)

  module Apply_infix = struct
    let ( <*> ) f a = apply a ~f

    let ( <**> ) a f = apply a ~f

    let ( *> ) a b = apply_const a b

    let ( <* ) a b = liftA2 const a b
  end

  include Apply_infix
end

module Make (X : Basic) : S with type 'a t := 'a X.t = struct
  include Make_general (struct
    type ('a, 'i, 'j, 'd, 'e) t = 'a X.t

    include (X : Basic with type 'a t := 'a X.t)
  end)

  include Functor.Make (X)
end

module Make2 (X : Basic2) : S2 with type ('a, 'e) t := ('a, 'e) X.t = struct
  include Make_general (struct
    type ('a, 'i, 'j, 'd, 'e) t = ('a, 'e) X.t

    include (X : Basic2 with type ('a, 'e) t := ('a, 'e) X.t)
  end)

  include Functor.Make2 (X)
end

module Make3 (X : Basic3) : S3 with type ('a, 'd, 'e) t := ('a, 'd, 'e) X.t =
struct
  include Make_general (struct
    type ('a, 'i, 'j, 'd, 'e) t = ('a, 'd, 'e) X.t

    include (X : Basic3 with type ('a, 'd, 'e) t := ('a, 'd, 'e) X.t)
  end)

  include Functor.Make3 (X)
end
