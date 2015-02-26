signature TOKEN =
sig
  structure Seq : SEQUENCE
  structure Elt : ELEMENT

  type token = Elt.t

  val tokenize : string -> token Seq.seq
  val compare : token * token -> order
  val toString : token -> string
  val toStringConcat : token Seq.seq -> string
end