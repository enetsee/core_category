include Alternative_intf

(* -- Helpers --------------------------------------------------------------- *)
module type Basic_general = sig
  type ('a, 'i, 'j, 'd, 'e) t

  val empty : ('a, 'i, 'j, 'd, 'e) t

  val alt :
    ('a, 'i, 'j, 'd, 'e) t -> ('a, 'i, 'j, 'd, 'e) t -> ('a, 'i, 'j, 'd, 'e) t
end

module Make_general (X : Basic_general) = struct
  let empty = X.empty

  let alt = X.alt

  module Alternative_infix = struct
    let ( <|> ) a b = alt a b
  end

  include Alternative_infix
end

(* -- Functors -------------------------------------------------------------- *)

module Make (X : Basic) : S with type 'a t := 'a X.t = struct
  include Make_general (struct
    type ('a, 'i, 'j, 'd, 'e) t = 'a X.t

    include (X : Basic with type 'a t := 'a X.t)
  end)

  include Applicative.Make (X)
end

(* -- Free Implementations -------------------------------------------------- *)

module Free = struct
  module Make (X : Functor.S) : FreeS with type 'a f := 'a X.t = struct
    type 'a t = 'a altF list

    and 'a altF =
      | Pure : 'a -> 'a altF
      | Ap : (('a -> 'b) t * 'a X.t) -> 'b altF

    let compose f g x = f (g x)

    let rec map_altF : 'a 'b. ('a -> 'b) -> 'a altF -> 'b altF =
     fun f x ->
      match x with
      | Pure a ->
          Pure (f a)
      | Ap (g, y) ->
          Ap (map_t (compose f) g, y)


    and map_t : 'a 'b. ('a -> 'b) -> 'a t -> 'b t =
     fun f x -> List.map (fun x -> map_altF f x) x


    let map x ~f = map_t f x

    let flip f y x = f x y

    (* let rec apply_altF : 'a 'b. ('a -> 'b) altF -> 'a altF -> 'b altF =
     fun af ax ->
      match (af, ax) with
      | Pure f, y ->
          map_altF f y
      | y, Pure a ->
          map_altF (fun f -> f a) y
      | Ap (f, a), b ->
          Ap (apply_t (map_t flip f) [ b ], a) *)

    let rec apply_t : 'a 'b. ('a -> 'b) t -> 'a t -> 'b t =
     fun fs xs -> fs |> List.map (fun f -> aux f xs) |> List.concat


    and aux : 'a 'b. ('a -> 'b) altF -> 'a t -> 'b t =
     fun af at ->
      match (af, at) with
      | Pure f, u ->
          map_t f u
      | Ap (f, u), v ->
          [ Ap (apply_t (map_t flip f) v, u) ]


    let apply x ~f = apply_t f x

    let pure_altF x = Pure x

    let pure x : 'a t = [ pure_altF x ]

    let empty : 'a t = []

    let alt x y : 'a t = List.append x y

    include Make (struct
      type nonrec 'a t = 'a t

      let map = map

      let map_const = `Define_using_map

      let pure = pure

      let apply = apply

      let empty = empty

      let alt = alt
    end)

    let liftAp (x : 'a X.t) : 'a t = [ Ap (pure (fun x -> x), x) ]
  end
end
