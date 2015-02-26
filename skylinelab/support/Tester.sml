structure Tester =
struct
  open StudentTestSuite

  type 'a sequence  = 'a ArraySequence.seq

  structure StuSkyline = MkSkyline (structure S = ArraySequence)

  structure OurSkyline = MkReferenceSkyline (structure S = ArraySequence)

  val tests = Tests.tests

  (* * * stringifier * * *)
  structure OutElt = MkPairElt (structure EltA = IntElt
                                structure EltB = IntElt)
  structure OutSeqElt = MkSeqElt (structure Elt = OutElt
                                  structure Seq = ArraySequence)

  structure InElt = MkTripleElt (structure EltA = IntElt
                                 structure EltB = IntElt
                                 structure EltC = IntElt)
  structure InSeqElt = MkSeqElt (structure Elt = InElt
                                  structure Seq = ArraySequence)

  val stringifier = Logger.create (InSeqElt.toString, OutSeqElt.toString)

  (* * * running the tests * * *)
  fun testSkyline () =
    let
      val checker = Checker.fromRefsol (StuSkyline.skyline,
                                        OurSkyline.skyline,
                                        OutSeqElt.equal)
    in
      Tester.testGroup checker stringifier tests
    end
end
