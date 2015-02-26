structure MkvcTester =
struct
  open StudentTestSuite

  structure Seq = ArraySequence
  val % = Seq.%

  structure HashKey = IntElt

  structure Rand : RANDOM210 where Seq = ArraySequence =
    MkMersenneTwister (structure Seq = ArraySequence)

  structure RTable : RTABLE where Seq = ArraySequence =
    MkBSTRTable (structure Tree = MkTreap (structure HashKey = HashKey)
                 structure Rand = Rand)

  structure Mkvc : MARKOV_CHAIN where RTable = RTable =
    MkRTableMarkovChain (structure RTable = RTable)

  structure RefMkvc : MARKOV_CHAIN where RTable = RTable =
    MkRefMarkovChain (structure RTable = RTable)

  (* UTILITIES =========================================================== *)
  fun pairToStr tos1 tos2 (x, y) =
    "(" ^ (tos1 x) ^ "," ^ (tos2 y) ^ ")"

  fun optionCmp cmp (NONE, NONE) = EQUAL
    | optionCmp cmp (NONE, _   ) = LESS
    | optionCmp cmp (_   , NONE) = GREATER
    | optionCmp cmp (SOME v1, SOME v2) = cmp (v1, v2)

  fun eq cmp (x, y) = cmp (x, y) = EQUAL

  fun both (x, y) = x andalso y

  fun tableCmp cmp (T1, T2) =
    let
      val T1' = RTable.map (fn v => (SOME v, NONE)) T1
      val T2' = RTable.map (fn v => (NONE, SOME v)) T2
      val together = RTable.merge (fn (v1, v2) => (#1 v1, #2 v2)) (T1', T2')
      val eqs = RTable.map (eq (optionCmp cmp)) together
    in
      if RTable.reduce both true eqs then EQUAL
      else LESS (* choose arbitrarily... shouldn't matter *)
    end

  fun seqCmpNoOrder cmp (S, T) =
    Seq.collate cmp (Seq.sort cmp S, Seq.sort cmp T)

  fun in2 f g (x, y) = (f x, g y)
  fun flip f (y, x) = f (x, y)

  (* TEST BUILD ========================================================== *)
  val stuBuild = Mkvc.build
  val refBuild = RefMkvc.build
  val buildEq = eq (tableCmp (seqCmpNoOrder HashKey.compare))

  val buildItos = Seq.toString (pairToStr HashKey.toString HashKey.toString)
  val buildOtos = RTable.toString (Seq.toString HashKey.toString)

  val buildChecker = Checker.fromRefsol (stuBuild, refBuild, buildEq)
  val buildLogger = Logger.create (buildItos, buildOtos)

  fun testBuild () =
    Tester.testGroup buildChecker buildLogger MkvcTests.chains

  (* TEST MERGE =========================================================== *)
  val stuMerge = Mkvc.merge o (in2 RefMkvc.build RefMkvc.build)
  val refMerge = RefMkvc.merge o (in2 RefMkvc.build RefMkvc.build)
  val mergeEq = eq (tableCmp (seqCmpNoOrder HashKey.compare))

  val mergeItos = pairToStr buildItos buildItos
  val mergeOtos = buildOtos

  val mergeChecker = Checker.fromRefsol (stuMerge, refMerge, mergeEq)
  val mergeLogger = Logger.create (mergeItos, mergeOtos)

  fun testMerge () =
    let
      val chains = MkvcTests.chains
      val numChains = length chains
      fun grabPair i j =
        (List.nth (chains, i), List.nth (chains, j))
      fun allPairs i =
        List.tabulate (numChains - i - 1, grabPair i)
      val tests = List.concat (List.tabulate (numChains - 1, allPairs))
        handle Size => (print "ERROR: Need more chains in MkvcTests.chains!\n";
                        raise Size)
    in
      Tester.testGroup mergeChecker mergeLogger tests
    end

  (* TEST SHRINK ========================================================== *)
  val stuShrink = Mkvc.shrink o RefMkvc.build
  val refShrink = RefMkvc.shrink o RefMkvc.build
  val shrunkEq = eq (tableCmp (tableCmp Real.compare))

  val shrinkItos = Seq.toString (pairToStr HashKey.toString HashKey.toString)
  val shrinkOtos = RTable.toString (RTable.toString Real.toString)

  val shrinkChecker = Checker.fromRefsol (stuShrink, refShrink, shrunkEq)
  val shrinkLogger = Logger.create (shrinkItos, shrinkOtos)

  fun testShrink () =
    Tester.testGroup shrinkChecker shrinkLogger MkvcTests.chains

  (* TEST ITER ============================================================ *)
  fun iter' f (S, (n, x)) =
    (Seq.rev o Seq.fromList)
    (f (flip op::) [] n (RefMkvc.build S) (Rand.fromInt x))

  fun iterVerify (_        , Result.Exn e     ) = false
    | iterVerify ((S,(n,x)), Result.Value outp) =
        if Seq.length outp = 0 then (n = 0 orelse Seq.length S = 0) else
        let
          val lenOut = Seq.length outp
          val shrunk = RefMkvc.shrink (RefMkvc.build S)
          fun grabPair i =
            (Seq.nth outp i, Seq.nth outp (i+1))
          val pairs = Seq.tabulate grabPair (lenOut - 1)

          fun checkPair (x1, x2) =
            isSome (RTable.find (valOf (RTable.find shrunk x1)) x2)
            handle Option => false
          val checkLast =
            case RTable.find shrunk (Seq.nth outp (lenOut - 1)) of
              NONE   => true
            | SOME T => RTable.size T = 0
        in
          (Seq.reduce both true (Seq.map checkPair pairs)) andalso
          (n = lenOut orelse checkLast)
        end

  val iterItos = pairToStr buildItos (pairToStr Int.toString Int.toString)
  val iterOtos = Seq.toString HashKey.toString

  val iterChecker = Checker.fromVerifier (iter' Mkvc.iter, iterVerify)
  val iterLogger = Logger.create (iterItos, iterOtos)

  fun testIter () =
    let
      val chains = MkvcTests.chains
      val inps = MkvcTests.iterInputs
      val tests = List.map (fn S => List.map (fn nx => (S, nx)) inps) chains
    in
      Tester.testGroup iterChecker iterLogger (List.concat tests)
    end

end

functor MkBabbleTester (structure Tok : TOKEN where Seq = ArraySequence) =
struct
  structure Seq = ArraySequence

  structure Rand : RANDOM210 where Seq = ArraySequence =
    MkMersenneTwister (structure Seq = ArraySequence)

  structure TokSeqElt : HASHKEY =
    MkSeqElt (structure Seq = ArraySequence
              structure Elt = Tok.Elt)

  structure RTable : RTABLE where Seq = ArraySequence =
    MkBSTRTable (structure Tree = MkTreap (structure HashKey = TokSeqElt)
                 structure Rand = Rand)

  structure Mkvc : MARKOV_CHAIN where Seq = ArraySequence =
    MkRTableMarkovChain (structure RTable = RTable)

  structure Babble : BABBLE where Seq = ArraySequence = 
    MkBabble (structure Tok = Tok
              structure Mkvc = Mkvc)

  fun mix files k =
    let
      fun read file = Tok.tokenize (TextIO.inputAll (TextIO.openIn file))
      val toks = Seq.map read (Seq.fromList files)
      val datas = Seq.map (fn toks => Babble.chainFromCorpus toks k) toks

      val emptymkvc = Babble.Mkvc.build (Seq.empty ())
      val data = Seq.reduce Babble.Mkvc.merge emptymkvc datas

      fun makeString len x =
        let
          val toks = Babble.babble data len (Rand.fromInt x)
        in
          Tok.toStringConcat toks ^ "\n"
        end
    in
      makeString
    end
end

structure CB =
  MkBabbleTester (structure Tok = MkCharToken (structure Seq = ArraySequence))
structure WB1 =
  MkBabbleTester (structure Tok = MkWordToken1 (structure Seq = ArraySequence))
structure WB2 =
  MkBabbleTester (structure Tok = MkWordToken2 (structure Seq = ArraySequence))
