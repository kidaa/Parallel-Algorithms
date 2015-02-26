(* Here's a crappy, "correct" implementation for finding MSTs. Note that this
 * does not meet the cost bounds, and does not segment graphs properly (it
 * ignores credit) *)
functor MkRefBoruvkaSegmenter
  (structure Seq : SEQUENCE
   structure Rand : RANDOM210
   sharing Seq = Rand.Seq)
  : SEGMENTER =
struct
  structure Seq = Rand.Seq
  open Seq

  type vertex = int
  type weight = int
  type edge = vertex * vertex * weight

  fun findSegments (E, n) _ =
    let
      fun allSame R =
        let
          val bottom = map (fn v => (v, true)) R
          val base = (nth R 0, true)
          fun combine ((v, a), (u, b)) =
            (v, v = u andalso a andalso b)
        in
          (#2 (reduce combine base bottom))
        end handle Range => true

      fun kruskalWouldNotBeProud (E, R, T) =
        if (allSame R) orelse (E = []) then (R, fromList T) else
        let
          val (u,v,w)::E' = E
          val repr = nth R
          fun redirect x =
            if (repr x) = (repr u) then (repr v) else (repr x)
          val R' = map redirect R
          val T' = if (repr u) = (repr v) then T else (u,v,w)::T
        in
          kruskalWouldNotBeProud (E', R', T')
        end

      fun edgeOrd ((_,_,w1),(_,_,w2)) = Int.compare (w1, w2)
      val presortedE = toList (sort edgeOrd E)
      val initR = tabulate (fn v => v) n
      val initT = []
    in
      kruskalWouldNotBeProud (presortedE, initR, initT)
    end
end
