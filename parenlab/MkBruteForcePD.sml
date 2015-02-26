functor MkBruteForcePD (structure P : PAREN_PACKAGE) : PAREN_DIST =
struct
  structure P = P
  open P
  open Seq

  (* Remove this line when you're done. *)
  exception NotYetImplemented

  (* subDist takes in a sequence of parenthesis and return the max
   * parenthesis dist that starts from the first element of S
   *)
  fun subDist S =
      let
        val n = length S
        (* Iteration function. Takes in level, dist and a charactor,
         * return level and dist after this charactor is added
         * level: layer of the parenthesis, 0 for closed, + for
         *        OPAREN, - for CPAREN
         * dist: length of the closed dist
         * Start with 0, when meet OPAREN, level+1, dist+1, when
         * meet CPAREN, level-1, dist+1
         * When level is back to 0 again, matched, the rest of the
         * string can does not change the result, return level=0 to
         * indicate this
         *)
        fun f ((level, dist), s) =
            if level = 0 then (0, dist) else
            if s = OPAREN then (level + 1, dist + 1) else
            (level - 1, dist + 1)
      in
        (* If the first element in the sequence is CPAREN, then
         * it cannot form a closed sequence starting from here
         * Otherwise, start the iter from this element to find
         * the longest closed sequence starting from here
         *)
        if (nth S 0) = OPAREN then
           let
             val (l, d) = (iter f (1, 1) (subseq S (1, n - 1)))
           in
             if l = 0 then d else 0
           end
        else 0
      end

  (* Brute Force
   * Find the longest closed parenthesis sequence starting from each
   * index, and find the longest one among them
   *)
  fun parenDist (parens : paren seq) : int option =
      let
        val n = length parens
      in
        (* Special case, empty string or only 1, both NONE *)
        if n = 0 orelse n = 1 then NONE else
        let
        (* If k = 2, then its dist is the max dist
         * Otherwise, max dist is the larger one of dist and
         * the recursive result of drop (S,1)
         *)
          fun max S =
              let
                val k = length S
                val dist = subDist S
              in
                if k <= 2 then dist else
                  let
                    val distOfSubseq = max (drop (S,1))
                  in
                    if dist > distOfSubseq then dist else distOfSubseq
                  end
              end
        in
          let
            val len = max parens
          in
            if len = 0 then NONE else SOME len
          end
        end
      end
end