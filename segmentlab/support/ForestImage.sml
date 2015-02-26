structure ForestImage =
struct
  open ArraySequence

  type pixel = { r : int, g : int, b : int }
  type image = { width : int, height : int, data : pixel seq seq }

  fun sumPixel ({r=r1, g=g1, b=b1} : pixel, {r=r2, g=g2, b=b2} : pixel) =
    {r=(r1 + r2), g=(g1 + g2), b=(b1 + b2)}

  fun divPixel ({r=r1, g=g1, b=b1} : pixel) n =
    {r=r1 div n, g=g1 div n, b=b1 div n}

  fun genImage (img as {width, height, data} : image) (vmap : int seq) =
    let
      val paired = mapIdx (fn (a, b) => (b, (a div width, 
                                            (a mod width))))
                       vmap
      val grouped = collect Int.compare paired
      fun avgPixel s = divPixel (reduce sumPixel {r=0, g=0, b=0} s) (length s)
      val coloring =
        map (fn (a, s) => (a, avgPixel s))
            (map (fn (a, s) => (a, map (fn (i, j) => nth (nth data i) j) 
                                        s)) 
                 grouped)
      val index = ref 0
      val mapped =
          tabulate
            (fn i =>
              if !index < length coloring
              then
                let
                  val (i', color) = nth coloring (!index)
                in
                  if i = i'
                  then (index := !index + 1; color)
                  else {r = 0, g = 0, b = 0}
                end
              else {r = 0, g = 0, b = 0})
            (width * height)
    in
      tabulate (fn i => 
        tabulate (fn j => nth mapped (nth vmap (width * i + j))) width) height
    end
end
