module type Basic = sig
  type t

  val append : t -> t -> t
end

module type Infix = sig
  type t

  val ( <> ) : t -> t -> t
end

module type S = sig
  include Basic

  module Semigroup_infix : Infix with type t := t

  include Infix with type t := t
end

module type Semigroup = sig
  module type Basic = Basic

  module type S = S

  module Make (X : Basic) : S with type t := X.t
end
