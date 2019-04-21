module type Basic = sig
  include Applicative_intf.Basic

  val empty : 'a t

  val alt : 'a t -> 'a t -> 'a t
end

module type Infix = sig
  type 'a t

  val ( <|> ) : 'a t -> 'a t -> 'a t
end

module type S = sig
  include Applicative_intf.S

  include Infix with type 'a t := 'a t

  module Alternative_infix : Infix with type 'a t := 'a t

  val empty : 'a t

  val alt : 'a t -> 'a t -> 'a t

  (* val some : 'a t -> 'a list t

  val many : 'a t -> 'a list t *)
end

module type FreeS = sig
  include S

  type 'a f

  val liftAp : 'a f -> 'a t
end

module type Free = sig
  module type FreeS = FreeS

  module Make (X : Functor_intf.S) : FreeS with type 'a f := 'a X.t
end

module type Alternative = sig
  module type Basic = Basic

  module type S = S

  module Make (X : Basic) : S with type 'a t := 'a X.t

  module type Free = Free
end
