functor MkBridges(structure STSeq : ST_SEQUENCE) : BRIDGES =
struct
  structure Seq = STSeq.Seq
  open Seq

  type vertex = int
  type edge = vertex * vertex
  type edges = edge seq
  type ugraph = vertex seq seq

  fun makeGraph (E : edge seq) : ugraph = 
      let
        fun input (g, (v1, v2)) =
            let
              val e1 = append ((nth g v1), %[v2])
              val e2 = append ((nth g v2), %[v1])
            in
              inject (%[(v1, e1), (v2, e2)]) g
            end
        val emptySeq = tabulate (fn _ => empty()) (2 * (length E))
        val g = iter input emptySeq E
      in
        filter (fn i => (length i) <> 0) g
      end

  fun findBridges (G : ugraph) : edges =
      let
        val vertexNum = length G
        val vertices = tabulate (fn i => i) vertexNum
        val parent = tabulate (fn _ => ~1) vertexNum
        val low = tabulate (fn _ => ~1) vertexNum
        val visitTime = tabulate (fn _ => ~1) vertexNum
        fun dfs (u, parent, low, visitTime, time, bridges) =
            let
              val visitTime = inject (%[(u, time)]) visitTime
              val low = inject (%[(u, time)]) low
              val time = time + 1
              val neighbors = nth G u
              fun visit ((parent, low, visitTime, time, bridges), v) =
                  if nth visitTime v = ~1 then
                    let
                      val parent = inject (%[(v, u)]) parent
                      val (parent, low, visitTime, time, bridges) =  
                          dfs (v, parent, low, visitTime, time, bridges)
                      val low = 
                          if nth low u > nth low v 
                          then inject (%[(u, nth low v)]) low
                          else low
                      val bridges = 
                          if nth low v > nth visitTime u 
                          then append (bridges, (%[(u, v)]))
                          else bridges
                    in
                      (parent, low, visitTime, time, bridges)
                    end
                  else if v <> nth parent u then
                    let
                      val update = Int.min(nth low u, nth visitTime v)
                      val low = inject (%[(u, update)]) low
                    in 
                      (parent, low, visitTime, time, bridges)
                    end
                        else (parent, low, visitTime, time, bridges)
            in
              iter visit (parent, low, visitTime, time, bridges) neighbors
            end
        fun dfsFrom ((parent, low, visitTime, time, bridges), u) =
            if nth parent u <> ~1 then (parent, low, visitTime, time, bridges)
            else dfs (u, parent, low, visitTime, time, bridges)
      in
        let
          val (_, _, _, _, b) = 
              iter dfsFrom (parent, low, visitTime, 0, (empty())) vertices
        in
          b
        end
      end
end
