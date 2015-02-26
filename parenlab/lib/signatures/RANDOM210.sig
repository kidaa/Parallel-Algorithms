signature RANDOM210 =
sig
  structure Seq : SEQUENCE

  type rand

  val fromInt : int -> rand

  val randomInt: rand -> ((int * int) option) -> (int * rand)
  val randomReal : rand -> ((real * real) option)-> (real * rand)

  val randomIntSeq: rand -> ((int * int) option) -> int -> (int Seq.seq * rand)
  val randomRealSeq: rand -> ((real * real) option) -> int -> (real Seq.seq * rand)

  val flip: rand -> int -> (int Seq.seq * rand)
end
