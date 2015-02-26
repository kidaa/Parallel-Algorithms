signature SEAMFIND =
sig

  structure Seq : SEQUENCE

  type 'a seq = 'a Seq.seq

  type pixel = { r : real, g : real, b : real }
  type image = { width : int, height : int, data : pixel seq seq }
  type gradient = real

  val findSeam : gradient seq seq -> int seq

end
