functor MkSequentialPD (structure P : PAREN_PACKAGE) : PAREN_DIST =
struct
  structure P = P
  open P
  open Seq

  fun parenDist (parens : paren seq) : int option =
      let
        (* pd : (int list * int option) option * (int * paren)
         *   -> (int list * int option) option
         *
         * If parens is a well-formed parenthesis sequence, then
         * pd maintains the state (SOME (opens, max)) while iterating
         * across the enumerated paren sequence, where:
         *
         *   (opens : int list) lists the positions of all unclosed OPAREN
         *   (max : int option) maintains the maximum paren dist found so far
         *)
        fun pd (([], max), (_, CPAREN)) = ([], max)
          | pd ((opens, max), (i, OPAREN)) = (i::opens, max)
          | pd ((j::opens, max), (i, CPAREN)) =
              (opens, Option210.intMax (max, SOME (i-j+1)))
      in
        case iter pd ([], NONE) (enum parens)
          of (_, SOME max) => SOME max
           | (_, NONE) => NONE
      end
end
