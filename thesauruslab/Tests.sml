structure Tests =
struct
  structure T = MkTreapTable(structure HashKey = IntElt)
  structure S = T.Seq
  open S

  type edge = int * int

  val testfile = "support/thesaurus.txt"

  (* A trivial test that has a graph containing 2 vertices and an edge *)
  val edgeseq = [(1,2),(1,3),(2,4),(3,5),(2,5),(3,4),(4,6),(5,6),(7,1)]
  
  (* Tests *)
  
  val testsNum = [edgeseq];

  val testsOutNeighbors = [(edgeseq, 6)]

  val testsReport = [((edgeseq, 1), 7)]

  val testsNumWords =  [testfile]

  val testsSynonyms = [(testfile, "HELLO")]

  val testsQuery = [(testfile, ("EARTHLY", "POISON"))]
end
