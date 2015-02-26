signature SEQFUN =
sig
  type 'a seq

  val allPerms : 'a seq -> 'a seq seq
  val permSort : ('a * 'a -> order) -> 'a seq -> 'a seq
end