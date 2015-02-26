signature RTABLE =
sig
  include TABLE
  structure Rand : RANDOM210
  sharing Seq = Rand.Seq
  val rselect : 'a table -> Rand.rand -> Key.t option
end