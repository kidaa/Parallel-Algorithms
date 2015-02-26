structure Tester =
struct
  open StudentTestSuite
  open Tests

  structure Seq = ArraySequence
  structure Tree = MkTreap (structure HashKey = IntElt)

  structure RefOrdTable = RefMkBSTOrderedTable (structure Tree = Tree
                                                structure Seq = Seq)
  structure RefRangeCount = RefMkRangeCount (structure OrdTable = RefOrdTable)

  structure StuOrdTable = MkBSTOrderedTable (structure Tree = Tree
                                                structure Seq = Seq)
  structure StuRangeCount = MkRangeCount (structure OrdTable = RefOrdTable)

  functor MkOT(structure OT : ORD_TABLE) =
  struct
    open OT.Seq

    fun first S =
      let
        val S' = OT.fromSeq (map (fn x => (x,())) S)
      in
        Option.map #1 (OT.first S')
      end

    fun last S =
      let
        val S' = OT.fromSeq (map (fn x => (x,())) S)
      in
        Option.map #1 (OT.last S')
      end

    fun next (S, k) =
      let
        val S' = OT.fromSeq (map (fn x => (x,())) S)
      in
        Option.map #1 (OT.next S' k)
      end

    fun previous (S, k) =
      let
        val S' = OT.fromSeq (map (fn x => (x,())) S)
      in
        Option.map #1 (OT.previous S' k)
      end

    fun join (S, T) =
      let
        val S' = (OT.fromSeq (map (fn x => (x,())) S))
        val T' = (OT.fromSeq (map (fn x => (x,())) T))
      in
        sort OT.Key.compare (map #1 (OT.toSeq (OT.join (S',T'))))
      end

    fun split (S, k) =
      let
        val S' = OT.fromSeq (map (fn x=> (x,())) S)
        val (L, m, R) = OT.split (S', k)
        val L' = sort OT.Key.compare (map #1 (OT.toSeq L))
        val R' = sort OT.Key.compare (map #1 (OT.toSeq R))
      in
        (L', m, R')
      end

    fun getRange (S, (l, h)) =
      let
        val S' = OT.fromSeq (map (fn x => (x,())) S)
      in
        sort OT.Key.compare
          (map #1 (OT.toSeq (OT.getRange S' (l,h))))
      end
  end

  structure StuOT = MkOT(structure OT = StuOrdTable)
  structure RefOT = MkOT(structure OT = RefOrdTable)

  structure IntSeqElt = MkSeqElt (structure Seq = Seq
                                  structure Elt = IntElt)
  structure IntSeqIntElt = MkPairElt (structure EltA = IntSeqElt
                                      structure EltB = IntElt)
  structure IntSeqIntSeqElt = MkPairElt (structure EltA = IntSeqElt
                                         structure EltB = IntSeqElt)
  structure IntOptionElt = MkOptionElt (structure Elt = IntElt)
  structure UnitOptionElt = MkOptionElt (structure Elt = UnitElt)
  structure IntSUnitIntSElt = MkTripleElt (structure EltA = IntSeqElt
                                         structure EltB = UnitOptionElt
                                         structure EltC = IntSeqElt)
  structure IntIntElt = MkPairElt (structure EltA = IntElt
                                   structure EltB = IntElt)
  structure IntSeqIntIntElt = MkPairElt (structure EltA = IntSeqElt
                                         structure EltB = IntIntElt)

  val flLogger = Logger.create(IntSeqElt.toString, IntOptionElt.toString)
  val pnLogger = Logger.create(IntSeqIntElt.toString, IntOptionElt.toString)
  val joinLogger = Logger.create(IntSeqIntSeqElt.toString, IntSeqElt.toString)
  val splitLogger = Logger.create(IntSeqIntElt.toString, IntSUnitIntSElt.toString)
  val rangeLogger = Logger.create(IntSeqIntIntElt.toString, IntSeqElt.toString)

  val firstChecker = Checker.fromRefsol(StuOT.first, RefOT.first, IntOptionElt.equal)
  val lastChecker = Checker.fromRefsol(StuOT.last, RefOT.last, IntOptionElt.equal)
  val prevChecker = Checker.fromRefsol(StuOT.previous, RefOT.previous, IntOptionElt.equal)
  val nextChecker = Checker.fromRefsol(StuOT.next, RefOT.next, IntOptionElt.equal)
  val joinChecker = Checker.fromRefsol(StuOT.join, RefOT.join, IntSeqElt.equal)
  val splitChecker = Checker.fromRefsol(StuOT.split, RefOT.split, IntSUnitIntSElt.equal)
  val rangeChecker = Checker.fromRefsol(StuOT.getRange, RefOT.getRange, IntSeqElt.equal)

  fun testFirst () =
    Tester.testGroup firstChecker flLogger Tests.testsFirst
  fun testLast () =
    Tester.testGroup lastChecker flLogger Tests.testsLast
  fun testPrev () =
    Tester.testGroup prevChecker pnLogger Tests.testsPrev
  fun testNext () =
    Tester.testGroup nextChecker pnLogger Tests.testsNext
  fun testJoin () =
    Tester.testGroup joinChecker joinLogger Tests.testsJoin
  fun testSplit () =
    Tester.testGroup splitChecker splitLogger Tests.testsSplit
  fun testRange () =
    Tester.testGroup rangeChecker rangeLogger Tests.testsRange

  functor MkRQ(structure RQ : RANGE_COUNT) =
  struct
    fun count (PS, (p1, p2)) =
      let
        val cTable = RQ.makeCountTable PS
      in
        RQ.count cTable (p1, p2)
      end
  end

  structure StuRange = MkRQ(structure RQ = StuRangeCount)
  structure RefRange = MkRQ(structure RQ = RefRangeCount)

  structure PointElt = MkPairElt (structure EltA = IntElt
                                  structure EltB = IntElt)
  structure PointPairElt = MkPairElt (structure EltA = PointElt
                                      structure EltB = PointElt)
  structure PointSeqElt = MkSeqElt (structure Seq = ArraySequence
                                    structure Elt = PointElt)
  structure CountInputElt = MkPairElt (structure EltA = PointSeqElt
                                       structure EltB = PointPairElt)

  val countLogger = Logger.create (CountInputElt.toString, IntElt.toString)
  val countChecker = Checker.fromRefsol (StuRange.count, RefRange.count, IntElt.equal)

  fun testCount () =
    Tester.testGroup countChecker countLogger Tests.testsCount
end
