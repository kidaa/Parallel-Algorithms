structure ImageGraph =
struct
  open ArraySequence

  type vertex = int
  type weight = int
  type edge = vertex * vertex * weight

  type pixel = { r : int, g : int, b : int }
  type image = { width : int, height : int, data : pixel seq seq }

  val sqrt = Real.round o Math.sqrt o Real.fromInt

  fun gradient' ({data, ...} : image) (i1, j1) (i2, j2) : int =
    let
      val {r=r1, g=g1, b=b1} = nth (nth data i1) j1
      val {r=r2, g=g2, b=b2} = nth (nth data i2) j2
      fun square x = x * x
    in
     (* sqrt *)((square (r1 - r2)) + (square (g1 - g2)) + (square (b1 - b2)))
    end

  fun gradient ({data, ...} : image) (i1, j1) (i2, j2) =
    let
      val {r=r1, g=g1, b=b1} = nth (nth data i1) j1
      val {r=r2, g=g2, b=b2} = nth (nth data i2) j2
      val b1 = Real.round (0.2126 * (Real.fromInt r1) +
                           0.7152 * (Real.fromInt g1) +
                           0.0722 * (Real.fromInt b1))
      val b2 = Real.round (0.2126 * (Real.fromInt r2) +
                           0.7152 * (Real.fromInt g2) +
                           0.0722 * (Real.fromInt b2))
    in 
      (b1 - b2) * (b1 - b2)
    end

  fun edgelist8 (img as {width, height, data} : image) n i j : edge list =
    case n of
      1 => if (i = 0)
           then edgelist8 img (n + 1) i j
           else (i * width + j,
                 (i - 1) * width + j,
                 gradient img (i, j) (i - 1, j)) ::
                edgelist8 img (n + 1) i j
    | 2 => if (i = 0) orelse (j = (width - 1))
           then edgelist8 img (n + 1) i j
           else (i * width + j,
                 (i - 1) * width + j + 1,
                 gradient img (i, j) (i - 1, j + 1)) ::
                edgelist8 img (n + 1) i j
    | 3 => if (j = width - 1)
           then edgelist8 img (n + 1) i j
           else (i * width + j,
                 i * width + j + 1,
                 gradient img (i, j) (i, j + 1)) ::
                edgelist8 img (n + 1) i j
    | 4 => if (i = height - 1) orelse (j = (width - 1))
           then edgelist8 img (n + 1) i j
           else (i * width + j,
                 (i + 1) * width + j + 1,
                 gradient img (i, j) (i + 1, j + 1)) ::
                edgelist8 img (n + 1) i j
    | 5 => if (i = height - 1)
           then edgelist8 img (n + 1) i j
           else (i * width + j,
                 (i + 1) * width + j,
                 gradient img (i, j) (i + 1, j)) ::
                edgelist8 img (n + 1) i j
    | 6 => if (i = height - 1) orelse (j = 0)
           then edgelist8 img (n + 1) i j
           else (i * width + j,
                 (i + 1) * width + j - 1,
                 gradient img (i, j) (i + 1, j - 1)) ::
                edgelist8 img (n + 1) i j
    | 7 => if (j = 0)
           then edgelist8 img (n + 1) i j
           else (i * width + j,
                 i * width + j - 1,
                 gradient img (i, j) (i, j - 1)) ::
                edgelist8 img (n + 1) i j
    | 8 => if (i = 0) orelse (j = 0)
           then edgelist8 img (n + 1) i j
           else (i * width + j,
                 (i - 1) * width + j - 1,
                 gradient img (i, j) (i - 1, j - 1)) ::
                edgelist8 img (n + 1) i j
    | _ => []

  fun edgelist4 (img as {width, height, data} : image) n i j : edge list =
    case n of
      1 => if (i = 0)
           then edgelist4 img (n + 1) i j
           else (i * width + j,
                 (i - 1) * width + j,
                 gradient img (i, j) (i - 1, j)) ::
                edgelist4 img (n + 1) i j
    | 2 => if (j = (width - 1))
           then edgelist4 img (n + 1) i j
           else (i * width + j,
                 i * width + j + 1,
                 gradient img (i, j) (i, j + 1)) ::
                edgelist4 img (n + 1) i j
    | 3 => if (i = height - 1)
           then edgelist4 img (n + 1) i j
           else (i * width + j,
                 (i + 1) * width + j,
                 gradient img (i, j) (i + 1, j)) ::
                edgelist4 img (n + 1) i j
    | 4 => if (j = 0)
           then edgelist4 img (n + 1) i j
           else (i * width + j,
                 i * width + j - 1,
                 gradient img (i, j) (i, j - 1)) ::
                edgelist4 img (n + 1) i j
    | _ => []

  fun genGraph8 (img as {width, height, data} : image) : edge seq =
    flatten
      (flatten
        (tabulate
          (fn i => tabulate
                     (fn j => fromList (edgelist8 img 1 i j))
                     width)
          height))


  fun genGraph4 (img as {width, height, data} : image) : edge seq =
    flatten
      (flatten
        (tabulate
          (fn i => tabulate
                     (fn j => fromList (edgelist4 img 1 i j))
                     width)
          height))

end
