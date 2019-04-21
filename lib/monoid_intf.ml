module type Basic = sig
  include Semigroup_intf.Basic

  val empty : t

  val concat : [ `Define_using_append | `Custom of t list -> t ]
end

module type S = sig
  include Semigroup_intf.S

  val empty : t

  val concat : t list -> t
end

module type Monoid = sig
  module type Basic = Basic

  module type S = S

  module Make (X : Basic) : S with type t := X.t
end
