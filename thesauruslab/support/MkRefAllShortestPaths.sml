functor MkRefAllShortestPaths (Table : TABLE) : ALL_SHORTEST_PATHS =
struct
  open Table
  open Seq

  exception NYI
  type nyi = unit

  (* Table.key defines our vertex type *)
  type vertex = key
  type edge = vertex * vertex

  type graph = edge seq
  type asp = graph * vertex

  (* Task 2.1 *)
  fun makeGraph (E : edge seq) : graph = E

  (* Task 2.2 *)
  fun numEdges (G : graph) : int = length G

  fun numVertices (G : graph) : int =
      Set.size (Set.fromSeq (append(map #1 G, map #2 G)))

  (* Task 2.3 *)
  fun outNeighbors (G : graph) (v : vertex) : vertex seq =
      map #2 (filter (fn (u,_) => Key.equal(u,v)) G)

  (* Task 2.4 *)
  fun makeASP (G : graph) (v : vertex) : asp = 
    let
      fun inGraph (G' : graph, v' : vertex) =
        let
          val edges = Seq.filter (fn (a,b) => Key.equal(a, v') orelse
                                              Key.equal(b, v')) G'
        in
          (length edges > 0)
        end
    in 
      if inGraph (G, v) then (G, v)
      else (Seq.empty (), v)
    end

  (* Task 2.5 *)
  fun report ((G, s) : asp) (t : vertex) : vertex seq seq =
      let
        fun lastElt S = nth S (length S - 1)

        fun pathHasTarget P = (Key.equal(lastElt P, t))

        fun report' (S : vertex seq seq) : vertex seq seq =
            let
              fun someFun (R : vertex seq) =
                  let
                    val nghs = outNeighbors G (lastElt R)
                  in
                    map (fn x => append(R, singleton x)) nghs
                  end

              val allPaths = flatten (map someFun S)
              val res = filter pathHasTarget S

              fun hasCrazyPaths P =
                  let
                    val longs = filter (fn p => length p > (numEdges G + 1)) P
                  in
                    length longs <> 0
                  end
            in
              if length res = 0
              then
               (if (length allPaths = 0) orelse (hasCrazyPaths S)
                then empty ()
                else report' allPaths)
              else res
            end
      in
        if (length G = 0) then Seq.empty () 
        else report' (singleton (singleton s))
      end

end
