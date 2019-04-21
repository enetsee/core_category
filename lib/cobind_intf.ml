module type Basic_extend = sig
  include Functor_intf.Basic

  val extend : 'a t -> f:('a t -> 'b) -> 'b t
end

module type Basic_duplicate = sig
  include Functor_intf.Basic

  val duplicate : 'a t -> 'a t t
end

module type Infix = sig
  type 'a t

  val ( =>> ) : 'a t -> ('a t -> 'b) -> 'b t

  val ( <<- ) : ('a t -> 'b) -> 'a t -> 'b t
end

module type S = sig
  include Functor_intf.S

  module Cobind_infix : Infix with type 'a t := 'a t

  include Infix with type 'a t := 'a t

  val extend : 'a t -> f:('a t -> 'b) -> 'b t

  val duplicate : 'a t -> 'a t t
end

module type Cobind = sig
  module type Basic_extend = Basic_extend

  module type Basic_duplicate = Basic_duplicate

  module type S = S

  module Make_extend (X : Basic_extend) : S with type 'a t := 'a X.t

  module Make_duplicate (X : Basic_duplicate) : S with type 'a t := 'a X.t
end
