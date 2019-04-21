module type Basic = sig
  include Apply_intf.Basic

  val bind : 'a t -> f:('a -> 'b t) -> 'b t

  val join : [ `Define_using_bind | `Custom of 'a t t -> 'a t ]
end

module type Infix = sig
  type 'a t

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

  val ( =<< ) : ('a -> 'b t) -> 'a t -> 'b t

  val ( >> ) : 'a t -> 'b t -> 'b t
end

module type S = sig
  include Apply_intf.S

  val bind : 'a t -> f:('a -> 'b t) -> 'b t

  val join : 'a t t -> 'a t

  include Infix with type 'a t := 'a t

  module Bind_infix : Infix with type 'a t := 'a t
end

module type Bind = sig
  module type Basic = Basic

  module type S = S

  module Make (X : Basic) : S with type 'a t := 'a X.t
end
