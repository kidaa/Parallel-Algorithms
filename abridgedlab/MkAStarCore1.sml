functor MkAStarCore(structure Table : TABLE
                    structure PQ : PQUEUE
                      where type Key.t = real) : ASTAR =
struct
  structure Set = Table.Set
  structure Seq = Set.Seq

  type weight = real
  type vertex = Set.key
  type edge = vertex * vertex * weight
  type heuristic = vertex -> real

  (* Define this type yourself *)
  type graph = weight Table.table Table.table

  fun makeGraph (E : edge Seq.seq) : graph = 
      let
        val kvSeq = Seq.map (fn (v1, v2, w) => (v1, (v2, w))) E
        val table = Table.collect kvSeq
      in
        Table.map Table.fromSeq table
      end

  fun findPath h G (S, T) =
      let
        val nS = Set.size S
        val S' = Set.toSeq S
        val (superS, s) = ((Seq.nth S' 0), Seq.subseq S' (1, nS-1))
        val T' = Set.toSeq T
        val nT = Set.size T
        val (superT, t) = ((Seq.nth T' 0), Seq.subseq T' (1, nT-1))

        val edgesS = Seq.map (fn i => (superS, (i, 0.0))) s
        val edgesT = Seq.map (fn i => (i, (superT, 0.0))) t
        val tableS = Table.map Table.fromSeq (Table.collect edgesS)
        val tableT = Table.map Table.fromSeq (Table.collect edgesT)

        val merge = Table.merge (fn (_, x) => x)
        val G' = Table.merge merge (G, tableS)
        val G' = Table.merge merge (G', tableT)



        val test = h(superS) + 0.0


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
        let
          val Result = dijkstra' (Table.empty ()) (PQ.singleton (0.0, superS))
        in
          Table.find Result superT
        end
      end

end