signature BABBLE =
sig
  structure Seq : SEQUENCE
  structure Tok : TOKEN
  structure Mkvc : MARKOV_CHAIN where type Key.t = Tok.token Seq.seq
  structure Rand : RANDOM210
  sharing Rand = Mkvc.Rand
  sharing Seq = Mkvc.Seq
  sharing Seq = Tok.Seq

  val chainFromCorpus : Tok.token Seq.seq -> int -> Mkvc.chain

  val babble : Mkvc.chain -> int -> Rand.rand -> Tok.token Seq.seq
end