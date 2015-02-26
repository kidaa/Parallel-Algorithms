functor MkDivideAndConquerPD (structure P : PAREN_PACKAGE) : PAREN_DIST =
struct
  structure P = P
  open Primitives
  open P
  open Seq

  (* Remove this line when you're done. *)
  exception NotYetImplemented

  (* Find the larger value of the two *)
  fun max2 (a,b) = if a > b then a else b

  (* Find the smaller value of the two *)
  fun min2 (a,b) = if a > b then b else a

  (* Divide and Conquor *)
  fun parenDist (parens : paren seq) : int option =
      let
        (* Recursive function pd takes in a sequence of parenthesis "seq"
         * and returns a tuple (p, s, m, t)
         * seq: input sequence of parenthesis
         * p: a list of int, the kth element represent the kth unmatched
         *    CPAREN counting from left side
         * s: a list of int, the kth element represent the kth unmatched
         *    OPAREN counting from right side
         * m: maximum parenthesis distance in "seq"
         * t: total length of "seq"
         *)
        fun pd seq =
            case showt seq
              of EMPTY => NONE
               | ELT OPAREN => SOME ([], [1], 0, 1)
               | ELT CPAREN => SOME ([1], [], 0, 1)
               | NODE (left, right) =>
                 let
                   (* Calculate two subseqs recursively in parallel *)
                   val (SOME (pl, sl, ml, tl), SOME (pr, sr, mr, tr)) =
                     par (fn () => pd left, fn () => pd right)
                   (* Add len to each element of list *)
                   fun app (list, len) =
                       List.map (fn i => i + len) list
                   (* Larger pd of subseqs *)
                   val m = max2 (ml, mr)
                   (* Length of this seq is sum of subseqs *)
                   val t = tl + tr
                 in
                   (* Append two subseqs and see if there's matches between
                    * subseqs. If none of two sides has unmatched parenthesis,
                    * update m and t only. If only one side has unmatched
                    * parenthesis, append to p or r. If both sides have
                    * unmatched parenthesis, combine and update p and r, then
                    * check if the combined new pd is larger than m
                    *)
                   case (sl, pr)
                     of ([], []) => SOME (pl, sr, m, t)
                      | (_, []) => SOME (pl, sr @ (app(sl, tr)), m, t)
                      | ([], _) => SOME (pl @ (app(pr, tl)), sr, m, t)
                      | (_, _) =>
                        let
                          fun combine (sl, pr) =
                              let
                                val n1 = List.length sl
                                val n2 = List.length pr
                                val mn = min2 (n1, n2)
                                val total_len =
                                  (List.nth (sl, (mn - 1)))
                                  + (List.nth (pr,(mn - 1)))
                              in
                                if n1 > n2
                                then ((List.drop (sl, n2)), [], total_len)
                                else ([], (List.drop (pr, n1)), total_len)
                              end
                          val (sl, pr, c) = combine (sl, pr)
                          val p = pl @ app (pr, tl)
                          val s = sr @ app (sl, tr)
                        in
                          SOME (p, s, max2 (m, c), t)
                        end
                 end
      in
        (* If get NONE, empty sequence, return NONE.
         * If get pd=0, unmatched sequence, return NONE.
         * Else, return the distance
         *)
        case (pd parens)
          of SOME (p, s, 0, t) => NONE
           | NONE => NONE
           | SOME (p, s, d, t) => SOME d
      end
end