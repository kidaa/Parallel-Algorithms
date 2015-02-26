structure Tests =
struct
  val testsAdd : (IntInf.int * IntInf.int) list = [
    (4000,3334),
    (3,2),
    (123, 937),
    (1,511),
    (0,0),
    (0,15),
    (31,31)
  ]

  val testsSub : (IntInf.int * IntInf.int) list = [
    (4000,3334),
    (3,2),
    (1024, 937),
    (13,5),
    (1,1),
    (0,0),
    (16,15)
  ]

  val testsMul : (IntInf.int * IntInf.int) list = [
    (4000,3334),
    (3,2),
    (123, 937),
    (1, 1),
    (15, 15),
    (0, 34)
  ]

end
