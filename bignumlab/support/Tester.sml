structure Tester =
struct
  open StudentTestSuite

  structure BNUtil = MkBigNumUtil(structure S = ArraySequence)

  structure OurBNA = MkBigNumAddRef(structure U = BNUtil)
  structure OurBNS = MkBigNumSubtractRef(structure U = BNUtil)
  structure OurBNM = MkBigNumMultiplyRef(structure U = BNUtil)

  structure StuBNA = MkBigNumAdd(structure U = BNUtil)
  structure StuBNS = MkBigNumSubtract(structure BNA = OurBNA)
  structure StuBNM = MkBigNumMultiply(structure BNA = OurBNA
                                      structure BNS = OurBNS)

  fun processTest (n1,n2) =
    (BNUtil.fromIntInf n1, BNUtil.fromIntInf n2)

  val testsAdd = List.map processTest Tests.testsAdd
  val testsSub = List.map processTest Tests.testsSub
  val testsMul = List.map processTest Tests.testsMul


  (* * * logger * * *)
  val outToStr = IntInf.toString o BNUtil.toIntInf
  fun inToStr (bn1, bn2) = "(" ^ (outToStr bn1) ^ ", " ^ (outToStr bn2) ^ ")"

  val logger = Logger.create (inToStr, outToStr)

  fun bignumEq (i1,i2) = (BNUtil.toIntInf i1) = (BNUtil.toIntInf i2)

  (* * * running the tests * * *)
  fun testAdd () =
    let
      val checker = Checker.fromRefsol (StuBNA.add,
                                        OurBNA.add,
                                        bignumEq)
    in
      Tester.testGroup checker logger testsAdd
    end

  fun testSub () =
    let
      val checker = Checker.fromRefsol (StuBNS.sub,
                                        OurBNS.sub,
                                        bignumEq)
    in
      Tester.testGroup checker logger testsSub
    end

  fun testMul () =
    let
      val checker = Checker.fromRefsol (StuBNM.mul,
                                        OurBNM.mul,
                                        bignumEq)
    in
      Tester.testGroup checker logger testsMul
    end
end
