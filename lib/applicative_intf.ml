module type Basic = sig
  include Apply_intf.Basic

  val pure : 'a -> 'a t
end

module type S = sig
  include Apply_intf.S

  val pure : 'a -> 'a t

  val whenA : bool -> unit t -> unit t
end

module type Basic2 = sig
  include Apply_intf.Basic2

  val pure : 'a -> ('a, _) t
end

module type S2 = sig
  include Apply_intf.S2

  val pure : 'a -> ('a, _) t

  val whenA : bool -> (unit, 'e) t -> (unit, 'e) t
end

module type FreeS = sig
  include S

  type 'a f

  val liftAp : 'a f -> 'a t
end

module type FreeS_Applicative = sig
  include FreeS

  val retractAp : 'a t -> 'a f
end

module type Free = sig
  module type FreeS = FreeS

  module type FreeS_Applicative = FreeS_Applicative

  module MakeOJ (X : Functor_intf.S) : FreeS with type 'a f := 'a X.t

  module MakeTvL (X : Functor_intf.S) : FreeS with type 'a f := 'a X.t

  module MakeFast (X : Functor_intf.S) : FreeS with type 'a f := 'a X.t

  module MakeFast_Applicative (X : S) :
    FreeS_Applicative with type 'a f := 'a X.t
end

module type Applicative = sig
  module type Basic = Basic

  module type Basic2 = Basic2

  module type S = S

  module type S2 = S2

  module Make (X : Basic) : S with type 'a t := 'a X.t

  module Make2 (X : Basic2) : S2 with type ('a, 'e) t := ('a, 'e) X.t

  module type Free = Free
end
