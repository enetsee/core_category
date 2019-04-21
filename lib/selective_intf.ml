module Selective_intf = struct
  module type Basic = sig
    include Applicative_intf.Basic

    val select : ('a, 'b) Either.t t -> ('a -> 'b) t -> 'b t
  end

  module type Infix = sig
    type 'a t

    val ( <*? ) : ('a, 'b) Either.t t -> ('a -> 'b) t -> 'b t

    val ( <||> ) : bool t -> bool t -> bool t

    val ( <&&> ) : bool t -> bool t -> bool t
  end

  module type S = sig
    include Applicative_intf.S

    include Infix with type 'a t := 'a t

    module Selective_infix : Infix with type 'a t := 'a t

    val select : ('a, 'b) Either.t t -> ('a -> 'b) t -> 'b t

    val branch : ('a, 'b) Either.t t -> ('a -> 'c) t -> ('b -> 'c) t -> 'c t

    val ifS : bool t -> 'a t -> 'a t -> 'a t

    val whenS : bool t -> unit t -> unit t
  end

  module type Selective = sig
    module type Basic = Basic

    module type S = S

    module Make (X : Basic) : S with type 'a t := 'a X.t
  end
end
