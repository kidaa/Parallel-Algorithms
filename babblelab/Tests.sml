structure MkvcTests =
struct
  structure Seq = ArraySequence
  val % = Seq.%

  (* build will be tested against individual inputs from chains
   *
   * merge will be tested against all possible pairs of inputs from chains
   *
   * shrink will be tested against individual inputs from chains
   *
   * iter will be tested against all possible pairs of inputs from both
   *   iterInputs and chains *)

  (* each element should be a valid input to Mkvc.build *)
  val chains : (int * int) Seq.seq list = [
    %[],
    %[(1,2),(2,3),(3,4),(4,1)],
    %[(1,1),(1,1),(1,2),(1,3),(2,1),(2,3),(2,3),(3,2)],
    %[(1,1),(2,2),(3,3),(4,4)],
    %[(1,1)],
    %[(1,2),(2,3),(3,4)]
  ]

  (* each element should be a pair of (n, x) which will be given to
   * Mkvc.iter f b n C (Rand.fromInt x)
   * (for some other values of f, b, C) *)
  val iterInputs : (int * int) list = [
    (10, 42),
    (2, 15210),
    (1, 513)
  ]
end