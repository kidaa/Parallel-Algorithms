signature SEGMENTER =
sig
  structure Seq : SEQUENCE

  type vertex = int
  type weight = int
  type edge = vertex * vertex * weight

  (* (findSegments (E, n) c) evaluates to (R, T)
   * where
   *   E is a sequence of edges (u, v, w)
   *   n is the number of vertices present in E (vertices are integers 0...n-1)
   *   c is the initial credit assigned to each vertex
   *
   *   R is a sequence mapping vertices to their component representative, and
   *   T is a sequence of tree edges that make up the computed MSTs.
   *)
  val findSegments : (edge Seq.seq * int) -> int -> (vertex Seq.seq * edge Seq.seq)
end
