module type Basic_extend = sig
  include Cobind_intf.Basic_extend

  val extract : 'a t -> 'a
end

module type Basic_duplicate = sig
  include Cobind_intf.Basic_duplicate

  val extract : 'a t -> 'a
end

module type S = sig
  include Cobind_intf.S

  val extract : 'a t -> 'a
end

module type Bind = sig
  module type Basic_extend = Basic_extend

  module type Basic_duplicate = Basic_duplicate

  module type S = S

  module Make_extend (X : Basic_extend) : S with type 'a t := 'a X.t

  module Make_duplicate (X : Basic_duplicate) : S with type 'a t := 'a X.t
end
