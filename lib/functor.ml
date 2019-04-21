include Functor_intf

module type Basic_general = sig
  type ('a, 'i, 'j, 'd, 'e) t

  val map : ('a, 'i, 'j, 'd, 'e) t -> f:('a -> 'b) -> ('b, 'i, 'j, 'd, 'e) t

  val map_const :
    [ `Define_using_map
    | `Custom of ('a, 'i, 'j, 'd, 'e) t -> 'b -> ('b, 'i, 'j, 'd, 'e) t
    ]
end

module Make_general (F : Basic_general) = struct
  let map = F.map

  let map_const_via_map fa b = F.map fa ~f:(fun _ -> b)

  let map_const =
    match F.map_const with
    | `Define_using_map ->
        map_const_via_map
    | `Custom f ->
        f


  let void fa = map_const fa ()

  module Functor_infix = struct
    let ( <$> ) f x = map x ~f

    let ( <&> ) x f = map x ~f

    let ( <$ ) b x = map_const x b

    let ( $> ) x b = map_const x b
  end

  include Functor_infix
end

module Make (F : Basic) : S with type 'a t := 'a F.t = Make_general (struct
  type ('a, 'i, 'j, 'd, 'e) t = 'a F.t

  include (F : Basic with type 'a t := 'a F.t)
end)

module Make2 (F : Basic2) : S2 with type ('a, 'd) t := ('a, 'd) F.t =
Make_general (struct
  type ('a, 'i, 'j, 'd, 'e) t = ('a, 'd) F.t

  include (F : Basic2 with type ('a, 'b) t := ('a, 'b) F.t)
end)

module Make3 (F : Basic3) : S3 with type ('a, 'd, 'e) t := ('a, 'd, 'e) F.t =
Make_general (struct
  type ('a, 'i, 'j, 'd, 'e) t = ('a, 'd, 'e) F.t

  include (F : Basic3 with type ('a, 'b, 'c) t := ('a, 'b, 'c) F.t)
end)
