signature ALL_SHORTEST_PATHS =
sig
  structure Seq : SEQUENCE

  type vertex
  type edge = vertex * vertex

  (* An internal graph type *)
  type graph

  (* The internal type to store all shortest paths *)
  type asp

  (* makeGraph E
   *   Takes a sequence of directed edges and generates a graph based on them.
   *   The number of vertices in the graph is equal to the number of
   *   unique vertex labels in the edge sequence.
   *)
  val makeGraph : edge Seq.seq -> graph

  (* Returns the number of directed edges *)
  val numEdges : graph -> int

  (* Returns the number of vertices *)
  val numVertices : graph -> int

  (* outNeighbors G v
   *   If v is in G, returns all out neighbors of v,
   *   otherwise returns an empty sequence.
   *)
  val outNeighbors : graph -> vertex -> vertex Seq.seq

  (* makeASP G v
   *   Given a graph and a vertex it generates an asp containing all the
   *   shortest paths from the vertex v to all other reachable vertices.
   *   If v is not in G, the asp will be empty.
   *)
  val makeASP : graph -> vertex -> asp

  (* report asp v
   *   Given an asp generated based on a vertex u,
   *   along with a vertex v, returns all the shortest paths from u to v
   *   as a sequence of sequence of vertices, where each sequence of vertices V
   *   represents a path of the form V = <u, w1, w2, ..., v > where each w in V
   *   only if w was originally a vertex in the input graph, and
   *   (Seq.nth V i-1, Seq.nth V i) is an edge in the original input graph for
   *   i in <1, 2, ..., (Seq.length V)-1>. Returns the empty sequence if there are
   *   no valid paths.
   *)
  val report : asp -> vertex -> vertex Seq.seq Seq.seq

end
