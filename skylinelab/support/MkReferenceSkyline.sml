functor MkReferenceSkyline(structure S : SEQUENCE) : SKYLINE =
struct
  structure Seq = S
  open Seq

  fun skyline buildings =
      if length buildings = 0 then empty ()
      else let
        fun minMax ((x1, y1), (x2, y2)) =
            (Int.min (x1, x2), Int.max (y1, y2))

        val lrs = map (fn (l,_,r) => (l,r)) buildings
        val (minX, maxX) = reduce minMax (nth lrs 0) lrs
        val xs = tabulate (fn i => minX + i) (maxX - minX + 1)

        fun maxHeight x =
            let
              fun inRange (l,_,r) = x >= l andalso x < r
              val heights = map #2 (filter inRange buildings)
            in reduce Int.max 0 heights
            end

        val range = map maxHeight xs
        fun isUniq (0, _) = true
          | isUniq (i, (x,h)) = nth range (i-1) <> h

      in filterIdx isUniq (zip xs range)
      end
end
