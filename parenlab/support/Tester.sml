structure Tester =
struct
  exception FatalError of string

  open StudentTestSuite
  open ArrayParenPackage

  type 'a sequence  = 'a Seq.seq

  structure BF = MkBruteForcePD (structure P = ArrayParenPackage)
  structure DC = MkDivideAndConquerPD (structure P = ArrayParenPackage)
  structure SQ = MkSequentialPD (structure P = ArrayParenPackage)

  (* * * stuff to make typing in tests easier * * *)
  fun iFromTC str =
    let
      fun parenFromC c =
        case c of
          #"(" => OPAREN
        | #")" => CPAREN
        | _ => raise FatalError "Tests must consist of only '(' and ')'."
    in
      ((Seq.map parenFromC) o Seq.% o String.explode) str
    end

  val tests = List.map iFromTC Tests.tests
  (* TODO add private tests here *)


  (* * * stringifier * * *)
  structure OutElt = MkOptionElt (structure Elt = IntElt)

  fun iToS ps =
    let
      fun parenToChar p =
        case p of
          OPAREN => #"("
        | CPAREN => #")"
    in
      (StringElt.toString o String.implode o Seq.toList o Seq.map parenToChar) ps
    end

  val stringifier = Logger.create (iToS, OutElt.toString)

  (* * * running the tests * * *)
  fun testBF () =
    let
      val checker = Checker.fromRefsol (BF.parenDist,
                                        SQ.parenDist,
                                        OutElt.equal)
    in
      Tester.testGroup checker stringifier tests
    end

  fun testDC () =
    let
      val checker = Checker.fromRefsol (DC.parenDist,
                                        SQ.parenDist,
                                        OutElt.equal)
    in
      Tester.testGroup checker stringifier tests
    end

end
