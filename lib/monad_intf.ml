module type Basic = sig
  include Bind_intf.Basic

  include Applicative_intf.Basic with type 'a t := 'a t
end

module type S = sig
  include Applicative_intf.S

  include
    Bind_intf.S
    with type 'a t := 'a t
     and module Functor_infix := Functor_infix
     and module Apply_infix := Apply_infix

  (* val return : 'a -> 'a t

  val sequence : 'a t list -> 'a list t *)
end

module type FreeS = sig
  include S

  type 'a f

  val impure : 'a t f -> 'a t

  val liftF : 'a f -> 'a t

  val run : ('a -> 'r) -> ('r f -> 'r) -> 'a t -> 'r
end

module type Free = sig
  module type FreeS = FreeS

  module Make (X : Basic) : FreeS with type 'a t := 'a X.t

  module MakeChurch (X : Basic) : FreeS with type 'a t := 'a X.t
end

module type Monad = sig
  module type Basic = Basic

  module type S = S

  module Make (X : Basic) : S with type 'a t := 'a X.t

  module type Free = Free
end
