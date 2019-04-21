module type Basic = sig
  include Functor_intf.Basic

  val apply : 'a t -> f:('a -> 'b) t -> 'b t
end

module type Infix = sig
  type 'a t

  val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t

  val ( <**> ) : 'a t -> ('a -> 'b) t -> 'b t

  val ( <* ) : 'a t -> 'b t -> 'a t

  val ( *> ) : 'a t -> 'b t -> 'b t
end

module type S = sig
  include Functor_intf.S

  include Infix with type 'a t := 'a t

  module Apply_infix : Infix with type 'a t := 'a t

  val apply : 'a t -> f:('a -> 'b) t -> 'b t

  val apply_const : 'a t -> 'b t -> 'b t

  val liftA2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
end

module type Basic2 = sig
  include Functor_intf.Basic2

  val apply : ('a, 'e) t -> f:('a -> 'b, 'e) t -> ('b, 'e) t
end

module type Infix2 = sig
  type ('a, 'e) t

  val ( <*> ) : ('a -> 'b, 'e) t -> ('a, 'e) t -> ('b, 'e) t

  val ( <**> ) : ('a, 'e) t -> ('a -> 'b, 'e) t -> ('b, 'e) t

  val ( <* ) : ('a, 'e) t -> ('b, 'e) t -> ('a, 'e) t

  val ( *> ) : ('a, 'e) t -> ('b, 'e) t -> ('b, 'e) t
end

module type S2 = sig
  include Functor_intf.S2

  include Infix2 with type ('a, 'e) t := ('a, 'e) t

  module Apply_infix : Infix2 with type ('a, 'e) t := ('a, 'e) t

  val apply : ('a, 'e) t -> f:('a -> 'b, 'e) t -> ('b, 'e) t

  val apply_const : ('a, 'e) t -> ('b, 'e) t -> ('b, 'e) t

  val liftA2 : ('a -> 'b -> 'c) -> ('a, 'e) t -> ('b, 'e) t -> ('c, 'e) t
end

module S_to_S2 (X : S) : S2 with type ('a, 'e) t = 'a X.t = struct
  type ('a, 'e) t = 'a X.t

  include (X : S with type 'a t := 'a X.t)
end

module S2_to_S (X : S2) : S with type 'a t = ('a, unit) X.t = struct
  type 'a t = ('a, unit) X.t

  include (X : S2 with type ('a, 'e) t := ('a, 'e) X.t)
end

module type Basic3 = sig
  include Functor_intf.Basic3

  val apply : ('a, 'd, 'e) t -> f:('a -> 'b, 'd, 'e) t -> ('b, 'd, 'e) t
end

module type Infix3 = sig
  type ('a, 'd, 'e) t

  val ( <*> ) : ('a -> 'b, 'd, 'e) t -> ('a, 'd, 'e) t -> ('b, 'd, 'e) t

  val ( <**> ) : ('a, 'd, 'e) t -> ('a -> 'b, 'd, 'e) t -> ('b, 'd, 'e) t

  val ( <* ) : ('a, 'd, 'e) t -> ('b, 'd, 'e) t -> ('a, 'd, 'e) t

  val ( *> ) : ('a, 'd, 'e) t -> ('b, 'd, 'e) t -> ('b, 'd, 'e) t
end

module type S3 = sig
  include Functor_intf.S3

  include Infix3 with type ('a, 'd, 'e) t := ('a, 'd, 'e) t

  module Apply_infix : Infix3 with type ('a, 'd, 'e) t := ('a, 'd, 'e) t

  val apply : ('a, 'd, 'e) t -> f:('a -> 'b, 'd, 'e) t -> ('b, 'd, 'e) t

  val apply_const : ('a, 'd, 'e) t -> ('b, 'd, 'e) t -> ('b, 'd, 'e) t

  val liftA2 :
    ('a -> 'b -> 'c) -> ('a, 'd, 'e) t -> ('b, 'd, 'e) t -> ('c, 'd, 'e) t
end

module S2_to_S3 (X : S2) : S3 with type ('a, 'd, 'e) t = ('a, 'd) X.t = struct
  type ('a, 'd, 'e) t = ('a, 'd) X.t

  include (X : S2 with type ('a, 'd) t := ('a, 'd) X.t)
end

module S3_to_S2 (X : S3) : S2 with type ('a, 'd) t = ('a, 'd, unit) X.t =
struct
  type ('a, 'd) t = ('a, 'd, unit) X.t

  include (X : S3 with type ('a, 'd, 'e) t := ('a, 'd, 'e) X.t)
end

module type Apply = sig
  module type Basic = Basic

  module type Basic2 = Basic2

  module type Basic3 = Basic3

  module type Infix = Infix

  module type Infix2 = Infix2

  module type Infix3 = Infix3

  module type S = S

  module type S2 = S2

  module type S3 = S3

  module Make (X : Basic) : S with type 'a t := 'a X.t

  module Make2 (X : Basic2) : S2 with type ('a, 'e) t := ('a, 'e) X.t

  module Make3 (X : Basic3) : S3 with type ('a, 'd, 'e) t := ('a, 'd, 'e) X.t
end
