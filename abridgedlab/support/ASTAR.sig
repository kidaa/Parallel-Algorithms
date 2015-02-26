signature ASTAR =
sig
  type vertex

  structure Set : SET where type Key.t = vertex

  type weight = real
  type edge = vertex * vertex * weight
  type heuristic = vertex -> real

  type graph

  (* Takes a sequence of directed edges as (u,v) pairs and returns the
   * corresponding graph.
   *)
  val makeGraph : edge Set.Seq.seq -> graph

  (* Given a heuristic h, a graph G, a set of source vertices S,
   * and a set of destination vertices T:
   *
   *   findPath h G (S, T) ==> res
   *
   * where res = SOME (v, dist) if the shortest S-T path has
   * ends at vertex v \in T with dist length,
   * or res = NONE if there is no S-T path in G.
   *)
  val findPath : heuristic -> graph -> (Set.set * Set.set)
                 -> (vertex * weight) option
end
