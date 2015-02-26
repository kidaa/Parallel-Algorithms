structure Tester :
sig
  val testMST : unit -> unit
  val testSegmenter : (string * string * int) -> unit
end =
struct
  open StudentTestSuite

  structure R = MkMersenneTwister (structure Seq = ArraySequence)
  open ArraySequence

  structure MSTTest =
  struct
    fun both (a, b) = a andalso b
    fun allTrue s = reduce both true s

    (* Undirects a sequence containly ONLY directed edges *)
    fun undirect E = flatten (map (fn (u,v,w) => %[(u,v,w),(v,u,w)]) E)

    (* Verifies that the edges in E connect all vertices in [0, n-1] *)
    fun isSpanning (E, n) =
        let
          val E' = map (fn (u,v,_) => (u,v)) (undirect E)
          val updates = collect Int.compare E'
          val G = inject updates (tabulate (fn _ => empty ()) n)
          fun BFS X F =
              if length F = 0 then allTrue X
              else let
                val X' = inject (map (fn v => (v, true)) F) X
                val F' = filter (not o (nth X')) (flatten (map (nth G) F))
              in BFS X' F'
              end
        in n = 0 orelse BFS (tabulate (fn _ => false) n) (singleton 0)
        end

    (* Verifies that the edges in E is a tree on the vertices in [0, n-1] *)
    fun isTree (E, n) : bool =
        (length E) = (n - 1)
  end
  
  (*structure StuBoruvkaMST = MkBoruvkaMST (structure Seq = ArraySequence structure Rand = Random210)*)
  structure StuBoruvkaSegmenter = MkBoruvkaSegmenter (structure Seq = ArraySequence
                                                      structure Rand = R)

  structure OurBoruvkaSegmenter = MkRefBoruvkaSegmenter (structure Seq = ArraySequence
                                                         structure Rand = R)

  structure EdgeElt = MkTripleElt(structure EltA = IntElt
                                  structure EltB = IntElt
                                  structure EltC = IntElt)
  structure EdgeSeqElt = MkSeqElt(structure Seq = ArraySequence
                                  structure Elt = EdgeElt)
  structure VertexSeqElt = MkSeqElt(structure Seq = ArraySequence
                                    structure Elt = IntElt)
  structure MSTInElt = MkTripleElt(structure EltA = EdgeSeqElt
                                   structure EltB = IntElt
                                   structure EltC = IntElt)
  structure MSTOutElt = MkPairElt(structure EltA = VertexSeqElt
                                  structure EltB = EdgeSeqElt)


  fun uncurry f (x, y, z) = f (x, y) z

  fun verify (_, Result.Exn _) = false
    | verify ((E, n, _), Result.Value (_, stus)) =
        let
          val bigCredit = valOf Int.maxInt
          val (_, ours) = OurBoruvkaSegmenter.findSegments (E, n) bigCredit
          val minWt = reduce op+ 0 (map #3 ours)
          val stuWt = reduce op+ 0 (map #3 stus)
        in
          minWt = stuWt andalso
          MSTTest.isSpanning (stus, n) andalso
          MSTTest.isTree (stus, n)
        end

  val checker = Checker.fromVerifier (uncurry StuBoruvkaSegmenter.findSegments, verify)
  val logger = Logger.create (MSTInElt.toString, MSTOutElt.toString)

  fun fixCase (L, n) =
    let
      fun delDupes eS =
        let
          fun sorter ((u,v,_), (u',v',_)) =
            case (Int.compare (u,u'), Int.compare (v,v')) of
              (EQUAL, x) => x
            | (x, _) => x
          val sorted = sort sorter eS
          fun filterer (i, (u,v,_)) =
            i = 0 orelse 
            let
              val (u',v',_) = nth sorted (i-1)
            in 
              u' <> u orelse v' <> v
            end
        in
          filterIdx filterer sorted
        end

      val delledL = delDupes L
      val L' = append (delledL, map (fn (u,v,w) => (v,u,w)) delledL)
    in
      (delDupes L', n, valOf Int.maxInt)
    end

  fun testMST () =
    let
      val tests = List.map (fixCase o (fn (E, n) => (% E, n))) Tests.testsMST
    in
      Tester.testGroup checker logger tests
    end



  structure Segmenter = StuBoruvkaSegmenter

  type pixel = { r : int, g : int, b : int }
  type image = { width : int, height : int, data : pixel seq seq }

  fun segment (img as {width, height, ...}, icredit) =
      if (icredit <= 0)
      then img
      else let
        val graph = ImageGraph.genGraph4 img
        val (segmented, _) = StuBoruvkaSegmenter.findSegments (graph, (width * height)) icredit
        val reconstructed = ForestImage.genImage img segmented
      in
        {width=width, height=height, data=reconstructed}
      end

  fun testSegmenter (inFile, outFile, k) =
      let
        val imageIn = ImageIO.fromFile inFile
        val imageOut = segment (imageIn, k)
      in
        ImageIO.toFile (outFile, imageOut)
      end
end
