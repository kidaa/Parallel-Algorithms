structure Tester =
struct
  open ArraySequence

  structure Bridges : BRIDGES =
    MkBridges(structure STSeq = MkSTSequence(structure Seq = ArraySequence))

  functor MkAStar(structure Vtx : HASHKEY) : ASTAR =
    MkAStarCore(structure Table = MkTreapTable(structure HashKey = Vtx)
                structure PQ = MkSkewBinomialHeapPQ(structure OrdKey = RealElt))

  structure IntAStar : ASTAR =
    MkAStar(structure Vtx = IntElt)

  structure StringAStar : ASTAR =
    MkAStar(structure Vtx = StringElt)


  structure OutElt = MkPairElt (structure EltA = IntElt
                                structure EltB = IntElt)
  structure InSeqElt = MkSeqElt (structure Elt = IntElt
                                 structure Seq = ArraySequence)

  val completGraph = [(0,1), (1,2), (1,3), (2,3),(0, 2), (0, 3), (0, 4), (1, 4), (2, 4), (3, 4)]
  val singletonGraph = []
  val simpleGraph = [(0, 1)]
  val wheelGraph = [(0, 1), (0, 2), (0,3), (0,4), (1, 2), (2, 3), (4, 5)]
  val circleGraph = [(0,1), (1,2), (2,3), (3, 4)]
  fun test1() =
    let
      val input = ArraySequence.% wheelGraph
      val g = Bridges.makeGraph input
      val output = Bridges.findBridges g
    in
      (*print (ArraySequence.toString OutElt.toString (ArraySequence.% [(1,2), (2,3), (3,1)]))*)
      
      (*print (ArraySequence.toString InSeqElt.toString g);*)
      print (ArraySequence.toString OutElt.toString output)
    end

  (*val simpleDG = [(0,1,0.1), (1,2,0.2), (1,3,0.3), (2,3,0.4)]
  fun test2() =
    let
      val input = ArraySequence.% simpleDG
      val g = MkAStar.makeGraph input
    in
      print "\n"
    end*)
end