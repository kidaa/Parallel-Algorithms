 functor MkAllShortestPaths (Table : TABLE) : ALL_SHORTEST_PATHS =
struct
  open Table
  open Seq

  (* Table.key defines our vertex type *)
  type vertex = key
  type edge = vertex * vertex

  (* Two tables: one stores out neighbors, the other in neighbors *)
  type graph = (((vertex seq) table) * ((vertex seq) table))
  (* A table from a vertex to a sequence of vetices which are next step on 
     the shortest path. (in reversed order) *)
  type asp = vertex seq table

  fun makeGraph (E : edge seq) : graph =
      let
        fun swap (k, v) = (v, k)
        val outTable = Table.collect E
        val inTable = Table.collect (Seq.map swap E)
      in
        (outTable, inTable)
      end

  fun numEdges (G : graph) : int = 
      let
        val (outTable, _) = G
        fun countNeighbors (count, (_, s)) = count + Seq.length s
      in
        Table.iter countNeighbors 0 outTable
      end

  fun numVertices (G : graph) : int =
      let
        val (outTable, inTable) = G
        val allV = Set.union (Table.domain outTable, Table.domain inTable)
      in
        Set.size allV
      end

  fun outNeighbors (G : graph) (v : vertex) : vertex seq =
      let
        val (outTable, _) = G
      in
        case Table.find outTable v
          of NONE => Seq.empty()
           | SOME seq => seq
      end

  fun makeASP (G : graph) (v : vertex) : asp =
      let
        val (outTable, inTable) = G 
        (* This function returns a table: NEXT frontier vertices -> 
           a seq of in-neighbors that are CURRENT frontier *)
        fun findNewFrontierEdges (g, xUf, f) =
            let
              fun ifNotInX u = not (Set.find xUf u)
              fun backtrack u =
                  let
                    val outN = Seq.filter ifNotInX (outNeighbors g u)
                  in
                    Seq.map (fn n => (n, u)) outN
                  end
              val base = Seq.empty()
              val newEdgesSeqs = (Seq.map backtrack (Set.toSeq f))
            in
              Table.collect (Seq.flatten newEdgesSeqs)
            end
        (* Update X, F, and add next steps to ASP *)
        fun findASP (X, F, ASP) = 
            if Set.size F = 0 then ASP
            else 
              let
                val newX = Set.union (X, F)
                val newEdgesInPath = findNewFrontierEdges (G, newX, F)
                val newF = Table.domain newEdgesInPath
                val newASP = Table.merge (fn (e, _) => e) (ASP, newEdgesInPath)
              in
                findASP (newX, newF, newASP)
              end
      in
        findASP (Set.empty(), Set.singleton(v), Table.empty())
      end           

  fun report (A : asp) (v : vertex) : vertex seq seq =
      let
        (* Find all possible next steps to form a shortest pathe, and append
           them to current path. *)
        fun findNextStep (path, s) =
            case Table.find A s
              of NONE => Seq.empty()
               | SOME next => 
                   let
                     val newPath = Seq.append (path, singleton(s))
                     fun format vertex = (newPath, vertex)
                   in
                     Seq.map format next
                  end
        (* Find next step to all possible paths. *)
        fun findPath currentPathsStatus =
            let
              val base = Seq.empty()
              val nextSteps = Seq.map findNextStep currentPathsStatus
              val newPathsStatus = Seq.flatten nextSteps
            in
              if Seq.length newPathsStatus =0 
              then currentPathsStatus
              else findPath newPathsStatus
            end
        fun formalize (seq, v) =
            Seq.rev (Seq.append (seq, singleton(v)))
        val start = Seq.singleton(Seq.empty(), v)
      in
        case Table.find A v
          of NONE => Seq.empty()
           | SOME _ => Seq.map formalize (findPath start)
      end
end
