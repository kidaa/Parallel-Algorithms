signature SKYLINE =
sig
  structure Seq : SEQUENCE

  val skyline : (int * int * int) Seq.seq -> (int * int) Seq.seq
end
