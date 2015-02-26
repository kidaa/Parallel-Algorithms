signature ELEMENT =
sig
  structure Hashing : HASHING
  type t
  val default : t
  val equal : t * t -> bool
  val compare : t * t -> order
  val hashgen : t -> Hashing.hashgen
  val hash : t -> int
  val toString : t -> string
end
