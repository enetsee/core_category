include Monad_intf

(* -- Functors -------------------------------------------------------------- *)

module Make (X : Basic) : S with type 'a t := 'a X.t = struct
  include (Bind.Make (X) : Bind.S with type 'a t := 'a X.t)

  include (
    Applicative.Make
      (X) :
      Applicative.S
      with type 'a t := 'a X.t
       and module Functor_infix := Functor_infix
       and module Apply_infix := Apply_infix )
end

(* -- Free Implementations -------------------------------------------------- *)

module Free = struct
  (** *)
  module MakeChurch (X : Functor_intf.S) : FreeS with type 'a f := 'a X.t =
  struct
    type 'a t = { unT : 'r. ('a -> 'r) -> ('r X.t -> 'r) -> 'r }

    let run kp kf f = f.unT kp kf

    let map x ~f = { unT = (fun kp kf -> run (fun y -> kp (f y)) kf x) }

    let apply x ~f =
      { unT = (fun kp kf -> run (fun g -> run (fun y -> kp (g y)) kf x) kf f) }


    let pure a = { unT = (fun kp _ -> kp a) }

    let bind m ~f =
      { unT = (fun kp kf -> run (fun x -> f x |> run kp kf) kf m) }


    include Make (struct
      type nonrec 'a t = 'a t

      let map = map

      let map_const = `Define_using_map

      let apply = apply

      let bind = bind

      let pure = pure

      let join =
        `Custom (fun mma -> { unT = (fun kp kf -> run (run kp kf) kf mma) })
    end)

    let impure (x : 'a t X.t) =
      { unT = (fun kp kf -> kf (X.map ~f:(fun m -> run kp kf m) x)) }


    let liftF x = impure (X.map ~f:pure x)

    (* let join (mma : 'a t t) = { apply = (fun kp kf -> run (run kp kf) kf mma) } *)
  end

  (** *)
  module Make (X : Functor_intf.S) : FreeS with type 'a f := 'a X.t = struct
    type 'a t =
      | Pure of 'a
      | Impure of 'a t X.t

    let rec map x ~f =
      match x with
      | Pure a ->
          Pure (f a)
      | Impure effect ->
          Impure (X.map effect ~f:(fun x -> map x ~f))


    let rec apply x ~f =
      match (f, x) with
      | Pure f, Pure x ->
          Pure (f x)
      | Pure f, Impure effect ->
          Impure (X.map effect ~f:(fun x -> map x ~f))
      | Impure ef, _ ->
          Impure (X.map ~f:(fun f -> apply ~f x) ef)


    let pure x = Pure x

    let rec bind m ~f =
      match m with
      | Pure a ->
          f a
      | Impure effect ->
          Impure (X.map ~f:(fun m -> bind m ~f) effect)


    include Make (struct
      type nonrec 'a t = 'a t

      let map = map

      let map_const = `Define_using_map

      let apply = apply

      let bind = bind

      let pure = pure

      let join = `Define_using_bind
    end)

    let rec run (p : 'a -> 'r) (i : 'r X.t -> 'r) f : 'r =
      match f with
      | Pure x ->
          p x
      | Impure effect ->
          i (X.map ~f:(run p i) effect)


    let impure x = Impure x

    let liftF effect = effect |> X.map ~f:pure |> impure
  end
end
