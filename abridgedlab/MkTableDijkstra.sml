(* This is here for your reference, and cannot actually be
 * used in any way. *)
functor MkTableDijkstra(structure Table : TABLE
                        structure PQ : PRIORITY_QUEUE
                          where type Key.t = real) =
struct
  structure Table = Table
  structure Set = Table.Set
  structure Seq = Set.Seq

  type weight = real
  type vertex = Set.key
  type graph = weight table table

  (* dijkstra (G, u) ==> D
   *
   * Solves the SSSP problem in graph G with u as the source vertex.
   * D maps every vertex in G to its shortest path distance from u.
   *)
  fun dijkstra (G, u) =
      let
        fun N(v) =
            case Table.find G v
              of NONE => Table.empty ()
               | SOME nbr => nbr

        fun dijkstra' D Q =
            case PQ.deleteMin Q
              of (NONE, _) => D
               | (SOME (d, v), Q') =>
                 case Table.find D v
                   of SOME _ => dijkstra' D Q'
                    | NONE =>
                      let
                        val insert = Table.insert (fn (x, _) => x)
                        val D' = insert (v, d) D
                        fun relax (q, (u, w)) = PQ.insert (d+w, u) q
                        val Q'' = Table.iter relax Q' (N v)
                      in dijkstra' D' Q''
                      end
      in
        dijkstra' (Table.empty ()) (PQ.singleton (0.0, u))
      end
end
