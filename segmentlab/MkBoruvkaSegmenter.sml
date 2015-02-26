functor MkBoruvkaSegmenter
  (structure Seq : SEQUENCE
   structure Rand : RANDOM210
   sharing Seq = Rand.Seq)
  : SEGMENTER =
struct
  structure Seq = Rand.Seq
  open Seq

  structure R = Rand
  type vertex = int
  type weight = int
  type edge = vertex * vertex * weight

  fun findSegments (E, n) initialCredit =
      let
        (* Opposite return value, so result is in descending order. *)
        fun reverseCmp ((_, _, w1), (_, _, w2)) =
                if w1 > w2 then LESS
                else if w1 = w2 then EQUAL
                     else GREATER
        val initialC = tabulate (fn i => initialCredit) n
        val sortedE = sort reverseCmp E
        val labeledE = map (fn (u, v, w) => (u, v, w, (u, v, w))) sortedE
        val initialSeed = Rand.fromInt 15210
        val NULL1 = (~1, ~1, (~1, ~1, ~1))
        val NULL2 = (~1, ~1, ~1, (~1, ~1, ~1))
        val emptyMinEdges = tabulate (fn i => NULL1) n
        fun minEdges E =
            let
              val minE = emptyMinEdges
              val update = map (fn (u, v, w, l) => (u, (v, w, l))) E
            in
              inject update minE
            end
        fun minStarContract (E, seed) = 
            let
              val (heads, newSeed) = Rand.flip seed n
              val minE = minEdges E
              fun findSatellites (u, (v, w, l)) = 
                  if v = ~1 then (NULL2)
                  else if (nth heads u = 0) andalso (nth heads v = 1)
                       then (u, v, w, l)
                       else NULL2
              val PT = filter (fn e => e <> NULL2) (mapIdx findSatellites minE)
            in
              (PT, newSeed)
            end
        fun MST (P, C, E, T, seed) =
            if length E = 0 then (P, T)
            else
              let
                val (PT, newSeed) = minStarContract (E, seed)
                fun merge (u, v, w, l) = if v = ~1 then u else v
                val P = map merge PT

                val maxInt = valOf (Int.maxInt);
                val credit = map (fn (u, v, w, l) => (v, nth C u)) PT
                val weight = map (fn (u, v, w, l) => (v, w)) PT
                val credit = filter (fn (v, _) => v <> ~1) credit
                val weight = filter (fn (v, _) => v <> ~1) weight
                val creditSeq = collect Int.compare credit
                val weightSeq = collect Int.compare weight
                fun findMinCredit (v, cs) = (v, reduce Int.min maxInt cs)
                fun findSumWeight (v, ws) = (v, reduce op+ 0 ws)
                fun calNewC ((v, minC), (_, sumW)) = (v, minC - sumW)
                val minCredit = map findMinCredit creditSeq
                val sumWeight = map findSumWeight weightSeq
                val update = map2 calNewC minCredit sumWeight
                val C' = inject update C

                val PT' = filter (fn (u, v, w, l) => v <> ~1) PT
                val T' = map (fn (u, v, w, l) => l) PT'
                val E' = filter (fn (u, v, w, l) => nth P u <> nth P v) E
                val E'' = map (fn (u, v, w, l) => (nth P u, nth P v, w, l)) E'
                fun negCreditCheck (u, v, w, _) = 
                    (Int.min(nth C u, nth C v) - w >= 0)
                val E''' = filter negCreditCheck E''
                val T'' = append (T, T')
              in
                MST (P, C, E''', T'', newSeed)
              end
        val (P, T) = MST (((empty())), initialC, labeledE, (empty()), initialSeed)
      in
        (P, T)
      end
end