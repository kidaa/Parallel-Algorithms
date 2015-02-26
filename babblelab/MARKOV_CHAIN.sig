signature MARKOV_CHAIN =
sig
  structure Seq : SEQUENCE
  structure RTable : RTABLE
  structure Rand : RANDOM210
  structure Key : EQKEY
  sharing Seq = Rand.Seq
  sharing Seq = RTable.Seq
  sharing Rand = RTable.Rand

  type chain = (Key.t Seq.seq) RTable.table

  val build : (Key.t * Key.t) Seq.seq -> chain
  val merge : chain * chain -> chain
  val shrink : chain -> real RTable.table RTable.table
  val iter : ('a * Key.t -> 'a) -> 'a -> int -> chain -> Rand.rand -> 'a
end