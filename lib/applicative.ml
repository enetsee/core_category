include Applicative_intf

(* -- Helpers --------------------------------------------------------------- *)
module type Basic_general = sig
  type ('a, 'i, 'j, 'd, 'e) t

  val pure : 'a -> ('a, _, _, _, _) t
end

module Make_general (X : Basic_general) = struct
  let pure = X.pure

  let whenA p s = if p then s else pure ()
end

(* -- Functors -------------------------------------------------------------- *)

module Make (X : Basic) : S with type 'a t := 'a X.t = struct
  include Make_general (struct
    type ('a, 'i, 'j, 'd, 'e) t = 'a X.t

    include (X : Basic with type 'a t := 'a X.t)
  end)

  include Apply.Make (X)
end

module Make2 (X : Basic2) : S2 with type ('a, 'e) t := ('a, 'e) X.t = struct
  include Make_general (struct
    type ('a, 'i, 'j, 'd, 'e) t = ('a, 'e) X.t

    include (X : Basic2 with type ('a, 'e) t := ('a, 'e) X.t)
  end)

  include Apply.Make2 (X)
end

(* -- Free Implementations -------------------------------------------------- *)

module Free = struct
  (*   Ørjan Johansen’s free applicative *)
  module MakeOJ (F : Functor_intf.S) : FreeS with type 'a f := 'a F.t = struct
    type 'a t =
      | Pure : 'a -> 'a t
      | Apply : (('a -> 'b) t * 'a F.t) -> 'b t

    let rec map_ : 'a 'b. 'a t -> ('a -> 'b) -> 'b t =
     fun x f ->
      match x with
      | Pure a ->
          Pure (f a)
      | Apply (tx, ay) ->
          Apply (map_ tx (fun g x -> f (g x)), ay)


    let map x ~f = map_ x f

    let compose f g x = f (g x)

    let rec ap_ : 'a 'b. ('a -> 'b) t -> 'a t -> 'b t =
     fun tx -> function
      | Pure y ->
          map tx ~f:(fun f -> f y)
      | Apply (ty, az) ->
          Apply (ap_ (map tx ~f:compose) ty, az)


    let apply x ~f = ap_ f x

    let pure x = Pure x

    include Make (struct
      type nonrec 'a t = 'a t

      let map = map

      let map_const = `Define_using_map

      let apply = apply

      let pure = pure
    end)

    let liftAp x = Apply (Pure (fun x -> x), x)
  end

  (* Twan van Laarhoven’s free applicative *)
  module MakeTvL (F : Functor_intf.S) : FreeS with type 'a f := 'a F.t = struct
    type 'a t =
      | Pure : 'a -> 'a t
      | Apply : ('a -> 'b) t * 'a F.t -> 'b t

    let pure x = Pure x

    let rec map_ : 'a 'b. ('a -> 'b) -> 'a t -> 'b t =
     fun f -> function
      | Pure a ->
          Pure (f a)
      | Apply (ga, v) ->
          Apply (map_ (fun g x -> f (g x)) ga, v)


    let map x ~f = map_ f x

    let flip f y x = f x y

    let rec ap_ : 'a 'b. ('a -> 'b) t -> 'a t -> 'b t =
     fun fa xa ->
      match fa with
      | Pure f ->
          map ~f xa
      | Apply (ga, v) ->
          Apply (ap_ (map ~f:flip ga) xa, v)


    let apply x ~f = ap_ f x

    include Make (struct
      type nonrec 'a t = 'a t

      let map = map

      let map_const = `Define_using_map

      let apply = apply

      let pure = pure
    end)

    let liftAp x = Apply (Pure (fun x -> x), x)
  end

  (* Dave Menendez's free applicative: https://www.eyrie.org/~zednenem/2013/05/27/freeapp *)
  module MakeFast_Base (X : Functor.Basic) = struct
    [@@@ocaml.warning "-37"]

    type 'a aseq =
      | ANil : unit aseq
      | ACons : ('a X.t * 'u aseq) -> ('a * 'u) aseq

    type ('y, 'z) fn = { apply : 'x. ('x -> 'y) -> 'x aseq -> 'z }

    let applyFn { apply } f s = apply f s

    type 'a t =
      { unap : 'u 'y 'z. ('y, 'z) fn -> ('u -> 'a -> 'y) -> 'u aseq -> 'z }

    let unAp { unap } fn k s = unap fn k s

    let pure a = { unap = (fun k f -> applyFn k (fun b -> f b a)) }

    let apply x ~f =
      { unap =
          (fun k e ->
            unAp
              x
              { apply = (fun h s -> unAp f k h s) }
              (fun s a g -> e s (g a)) )
      }


    let map x ~f = { unap = (fun k g -> unAp x k (fun s a -> g s (f a))) }

    include Make (struct
      type nonrec 'a t = 'a t

      let map = map

      let map_const = `Define_using_map

      let apply = apply

      let pure = pure
    end)

    let liftAp a =
      { unap = (fun k f s -> applyFn k (fun (a', s') -> f s' a') (ACons (a, s)))
      }
  end

  module MakeFast (X : Functor.Basic) : FreeS with type 'a f := 'a X.t = struct
    include MakeFast_Base (X)
  end

  module MakeFast_Applicative (X : Applicative_intf.Basic) :
    FreeS_Applicative with type 'a f := 'a X.t = struct
    include MakeFast_Base (X)

    let tuple2 x y = (x, y)

    (** Interprets the sequence of effects using the semantics for
        `pure` and `<*>` given by the Applicative instance for 'X.t'.
    *)
    let rec reduceASeq : type u. u aseq -> u X.t =
     fun s ->
      match s with
      | ANil ->
          X.pure ()
      | ACons (next, rest) ->
          X.(
            let f = map next ~f:tuple2 in
            apply (reduceASeq rest) ~f)


    let retractAp x : 'a X.t =
      unAp
        x
        { apply = (fun f s -> X.map ~f (reduceASeq s)) }
        (fun _ x -> x)
        ANil
  end
end
