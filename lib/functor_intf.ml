module type Basic = sig
  type 'a t

  val map : 'a t -> f:('a -> 'b) -> 'b t

  val map_const : [ `Define_using_map | `Custom of 'a t -> 'b -> 'b t ]
end

module type Infix = sig
  type 'a t

  val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t

  val ( <&> ) : 'a t -> ('a -> 'b) -> 'b t

  val ( <$ ) : 'b -> 'a t -> 'b t

  val ( $> ) : 'a t -> 'b -> 'b t
end

module type S = sig
  include Infix

  module Functor_infix : Infix with type 'a t := 'a t

  val map : 'a t -> f:('a -> 'b) -> 'b t

  val map_const : 'a t -> 'b -> 'b t

  val void : 'a t -> unit t
end

module type Basic2 = sig
  type ('a, 'e) t

  val map : ('a, 'e) t -> f:('a -> 'b) -> ('b, 'e) t

  val map_const :
    [ `Define_using_map | `Custom of ('a, 'e) t -> 'b -> ('b, 'e) t ]
end

module type Infix2 = sig
  type ('a, 'e) t

  val ( <$> ) : ('a -> 'b) -> ('a, 'e) t -> ('b, 'e) t

  val ( <&> ) : ('a, 'e) t -> ('a -> 'b) -> ('b, 'e) t

  val ( <$ ) : 'b -> ('a, 'e) t -> ('b, 'e) t

  val ( $> ) : ('a, 'e) t -> 'b -> ('b, 'e) t
end

module type S2 = sig
  include Infix2

  module Functor_infix : Infix2 with type ('a, 'e) t := ('a, 'e) t

  val map : ('a, 'e) t -> f:('a -> 'b) -> ('b, 'e) t

  val map_const : ('a, 'e) t -> 'b -> ('b, 'e) t

  val void : ('a, 'e) t -> (unit, 'e) t
end

module S_to_S2 (F : S) : S2 with type ('a, 'e) t = 'a F.t = struct
  type ('a, 'e) t = 'a F.t

  include (F : S with type 'a t := 'a F.t)
end

module S2_to_S (F : S2) : S with type 'a t = ('a, unit) F.t = struct
  type 'a t = ('a, unit) F.t

  include (F : S2 with type ('a, 'e) t := ('a, 'e) F.t)
end

module type Basic3 = sig
  type ('a, 'd, 'e) t

  val map : ('a, 'd, 'e) t -> f:('a -> 'b) -> ('b, 'd, 'e) t

  val map_const :
    [ `Define_using_map | `Custom of ('a, 'd, 'e) t -> 'b -> ('b, 'd, 'e) t ]
end

module type Infix3 = sig
  type ('a, 'd, 'e) t

  val ( <$> ) : ('a -> 'b) -> ('a, 'd, 'e) t -> ('b, 'd, 'e) t

  val ( <&> ) : ('a, 'd, 'e) t -> ('a -> 'b) -> ('b, 'd, 'e) t

  val ( <$ ) : 'b -> ('a, 'd, 'e) t -> ('b, 'd, 'e) t

  val ( $> ) : ('a, 'd, 'e) t -> 'b -> ('b, 'd, 'e) t
end

module type S3 = sig
  include Infix3

  module Functor_infix : Infix3 with type ('a, 'd, 'e) t := ('a, 'd, 'e) t

  val map : ('a, 'd, 'e) t -> f:('a -> 'b) -> ('b, 'd, 'e) t

  val map_const : ('a, 'd, 'e) t -> 'b -> ('b, 'd, 'e) t

  val void : ('a, 'd, 'e) t -> (unit, 'd, 'e) t
end

module S2_to_S3 (F : S2) : S3 with type ('a, 'd, 'e) t = ('a, 'd) F.t = struct
  type ('a, 'd, 'e) t = ('a, 'd) F.t

  include (F : S2 with type ('a, 'd) t := ('a, 'd) F.t)
end

module S3_to_S2 (F : S3) : S2 with type ('a, 'd) t = ('a, 'd, unit) F.t =
struct
  type ('a, 'd) t = ('a, 'd, unit) F.t

  include (F : S3 with type ('a, 'd, 'e) t := ('a, 'd, 'e) F.t)
end

module type Functor = sig
  module type Basic = Basic

  module type Basic2 = Basic2

  module type Basic3 = Basic3

  module type Infix = Infix

  module type Infix2 = Infix2

  module type Infix3 = Infix3

  module type S = S

  module type S2 = S2

  module type S3 = S3

  module Make (F : Basic) : S with type 'a t := 'a F.t

  module Make2 (F : Basic2) : S2 with type ('a, 'e) t := ('a, 'e) F.t

  module Make3 (F : Basic3) : S3 with type ('a, 'd, 'e) t := ('a, 'd, 'e) F.t
end
