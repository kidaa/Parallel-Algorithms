functor MkSkyline(structure S : SEQUENCE) : SKYLINE =
struct
  structure Seq = S
  open Primitives
  open Seq

  (* Remove this line when you're done. *)
  exception NotYetImplemented

  fun skyline (buildings : (int * int * int) seq) : (int * int) seq =
      (* This is the template we recommend you use for your divide
       * and conquer algorithm. You do NOT have to use it, however.
       * Delete this comment when you have completed skyline.
       *)
      let
        fun computeSkyline (S : (int * int * int) seq) : (int * int) seq =
            case showt S of
              EMPTY => singleton (0, 0)
            | ELT (l, h, r) => %[(l, h), (r, 0)]
            | NODE(L, R) => 
            let
              (* compare the position of two points *)
              fun cmp ((x1, h1), (x2, h2)) = 
                  if x1 < x2
                  then LESS
                  else if x1 = x2
                       then EQUAL
                       else GREATER
              (* this function maps all the -1 height to the element right
                 before it *)
              fun copy ((b_x, b_h), (s_x, s_h)) = 
                  if s_h = ~1 then (s_x, b_h) else (s_x, s_h)
              (* this function returns the tuple with greater height *)
              fun max ((l_x, l_h), (r_x, r_h)) = 
                  if l_h > r_h then (l_x, l_h) else (r_x, r_h)
              (* for the filter, false means this tuple should be deleted *)
              fun remove (x, h) = 
                  if x = ~1 then false else true
              (* recursion *)
              val (left, right) = 
                  par (fn () => computeSkyline L, fn () => computeSkyline R)
              (* create two sequences that only contains position info *)
              val left_pos = map (fn (x, h) => (x, ~1)) left
              val right_pos = map (fn (x, h) => (x, ~1)) right
              (* merge a sub-skyline with the other's position info *)
              val skyline_l = merge cmp left right_pos
              val skyline_r = merge cmp left_pos right
              (* find those -1 heights and set it to previous value *)
              val skyline_l = scani copy (0, ~1) skyline_l
              val skyline_r = scani copy (0, ~1) skyline_r
              (* combine two sub-skylines by choosing the larger value on
                 each point *)
              val skyline = map2 max skyline_l skyline_r
              val skyline1 = drop (skyline, 1)
              val skyline2 = take (skyline, ((length skyline)-1))
              fun label ((b1,h1),(b2,h2)) = 
                  if h1=h2 then (~1,h2) else (b1,h1)
              val skyline3 = 
                  append ((singleton (nth skyline 0)), 
                    (map2 label skyline1 skyline2))
              (* label those has same height with previous one 
              val skyline3 = scani label (~1, ~1) skyline*)
            in
              (* filter those labelled *)
              filter remove skyline3
            end
      in
        computeSkyline buildings
      end

end
