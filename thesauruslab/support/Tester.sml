structure Tester =
struct
  open StudentTestSuite
  structure Seq = ArraySequence

  functor MkTestables (structure Asp : ALL_SHORTEST_PATHS structure The : THESAURUS) =
  struct
    open Asp
    open The
    structure Utils = MkThesaurusUtils(The.Seq)

    fun uncurry f (a, b) = f a b
    fun uncurrySkip f a (b, c) = f a b c

    val numEdges =
        numEdges o makeGraph
    val numVertices =
        numVertices o makeGraph
    val outNeighbors =
        (Asp.Seq.sort Int.compare) o Unsafe.cast o
        (uncurry (outNeighbors o makeGraph))
    val report : ((edge Asp.Seq.seq * Asp.vertex) * Asp.vertex) -> int Asp.Seq.seq Asp.Seq.seq =
        (Asp.Seq.sort (Asp.Seq.collate Int.compare)) o Unsafe.cast o
        (uncurry (report o (uncurry (makeASP o makeGraph))))
    val numWords =
        numWords o (make o (Utils.fromFile))
    val synonyms =
        (The.Seq.sort String.compare) o
        (uncurry (synonyms o (make o (Utils.fromFile))))
    val query =
        (uncurry (uncurrySkip (query o (make o (Utils.fromFile)))))
  end

  structure IntTable : TABLE  =
      MkTreapTable(structure HashKey = IntElt)
  structure StringTable : TABLE  =
      MkTreapTable(structure HashKey = StringElt)

  structure StuAllShortestPaths = MkAllShortestPaths(IntTable)
  structure StuThesaurusASP = MkThesaurusASP(MkAllShortestPaths(StringTable))
  structure StuTestables = MkTestables (structure Asp = StuAllShortestPaths
                                        structure The = StuThesaurusASP)

  structure OurAllShortestPaths = MkRefAllShortestPaths(IntTable)
  structure OurThesaurusASP = MkRefThesaurusASP(MkRefAllShortestPaths(StringTable))
  structure OurTestables = MkTestables (structure Asp = OurAllShortestPaths
                                        structure The = OurThesaurusASP)

  (* * * test cases * * *)
  val testsNum =
    List.map Seq.% Tests.testsNum
  val testsOutNeighbors =
    List.map (fn (x,y) => (Seq.% x,y)) Tests.testsOutNeighbors
  val testsReport =
    List.map (fn ((x,y),z) => ((Seq.% x,y),z)) Tests.testsReport
  val testsNumWords =
    Tests.testsNumWords
  val testsSynonyms =
    Tests.testsSynonyms
  val testsQuery =
    Tests.testsQuery

  (* * * loggers * * *)
  structure EdgeElt = MkPairElt(structure EltA = IntElt
                                structure EltB = IntElt)
  structure EdgeSeqElt = MkSeqElt(structure Elt = EdgeElt
                                  structure Seq = ArraySequence)
  structure EdgeSeqVertElt = MkPairElt(structure EltA = EdgeSeqElt
                                       structure EltB = IntElt)
  structure IntSeqElt = MkSeqElt(structure Elt = IntElt
                                 structure Seq = ArraySequence)
  structure StringPairElt = MkPairElt(structure EltA = StringElt
                                      structure EltB = StringElt)
  structure StringSeqElt = MkSeqElt(structure Elt = StringElt
                                    structure Seq = ArraySequence)

  structure NumOut = IntElt
  structure NumIn = EdgeSeqElt
  structure OutNeighborsOut = IntSeqElt
  structure OutNeighborsIn = EdgeSeqVertElt
  structure ReportIn = MkPairElt(structure EltA = EdgeSeqVertElt
                                  structure EltB = IntElt)
  structure ReportOut = MkSeqElt(structure Elt = IntSeqElt
                                 structure Seq = ArraySequence)
  structure NumWordsOut = IntElt
  structure NumWordsIn = StringElt
  structure SynonymsOut = StringSeqElt
  structure SynonymsIn = StringPairElt
  structure QueryOut = MkSeqElt(structure Elt = StringSeqElt
                               structure Seq = ArraySequence)
  structure QueryIn = MkPairElt(structure EltA = StringElt
                                 structure EltB = StringPairElt)


  (* * * checkers * * *)
  val numEdgesSetup =
    (Checker.fromRefsol (StuTestables.numEdges,
                         OurTestables.numEdges,
                         NumOut.equal),
     Logger.create (NumIn.toString, NumOut.toString))
  val numVerticesSetup =
    (Checker.fromRefsol (StuTestables.numVertices,
                         OurTestables.numVertices,
                         NumOut.equal),
     Logger.create (NumIn.toString, NumOut.toString))
  val outNeighborsSetup =
    (Checker.fromRefsol (StuTestables.outNeighbors,
                         OurTestables.outNeighbors,
                         OutNeighborsOut.equal),
     Logger.create (OutNeighborsIn.toString, OutNeighborsOut.toString))
  val reportSetup =
    (Checker.fromRefsol (StuTestables.report,
                         OurTestables.report,
                         ReportOut.equal),
     Logger.create (ReportIn.toString, ReportOut.toString))
  val numWordsSetup =
    (Checker.fromRefsol (StuTestables.numWords,
                         OurTestables.numWords,
                         NumWordsOut.equal),
     Logger.create (NumWordsIn.toString, NumWordsOut.toString))
  val synonymsSetup =
    (Checker.fromRefsol (StuTestables.synonyms,
                         OurTestables.synonyms,
                         SynonymsOut.equal),
     Logger.create (SynonymsIn.toString, SynonymsOut.toString))

  (* * * running the tests * * *)
  fun runTests (checker, logger) tests =
    Tester.testGroup checker logger tests

  fun testNumEdges () =
    runTests numEdgesSetup testsNum
  fun testNumVertices () =
    runTests numVerticesSetup testsNum
  fun testOutNeighbors () =
    runTests outNeighborsSetup testsOutNeighbors
  fun testReport () =
    runTests reportSetup testsReport
  fun testNumWords () =
    runTests numWordsSetup testsNumWords
  fun testSynonyms () =
    runTests synonymsSetup testsSynonyms
  fun testQuery () =
    let
      val results = Seq.flatten (Seq.% (List.map StuTestables.query testsQuery))
      val strings = Seq.map (String.concatWith " -> " o Seq.toList) results
    in
      print ((String.concatWith "\n" (Seq.toList strings)) ^ "\n\n")
    end
end
