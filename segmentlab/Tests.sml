structure Tests =
struct
  (* testsMST: list of (E, n) pairs, where E is a list of edges (u,v,w) and
   *   n is the number of vertices
   *
   * note: sum of edge weights must be strictly less than the
   *   maximum integer (valOf Int.maxInt) *)
  val testsMST =
    [
      (*([(0,2,10), (1,0,5)], 3),*)
      ([(0,1,3), (0,2,6), (1,3,2), (1,4,9), 
      	(2,3,2), (2,6,9), (3,5,8), (4,5,8), 
      	(4,9,18), (5,6,7), (5,8,9), (6,7,4),
      	(6,8,5), (7,8,1), (7,9,4), (8,9,3)], 32)
    ]
end
