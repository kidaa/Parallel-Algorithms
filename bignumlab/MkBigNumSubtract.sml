functor MkBigNumSubtract(structure BNA : BIGNUM_ADD) : BIGNUM_SUBTRACT =
struct
  structure Util = BNA.Util
  open Util
  open Seq

  (* Remove this line when you're done. *)
  exception NotYetImplemented

  infix 6 ++ --

  fun x ++ y = BNA.add (x, y)
  fun x -- y =
      let
        (* Extend y to the length of x and get ~y *)
        val nx = length x
        val ny = length y
        val extended_y = append (y, tabulate (fn i => ZERO) (nx - ny))
        val not_y = map (fn bit => if bit = ONE then ZERO else ONE) extended_y
        (* Use two's complement to do the substract. Drop sign bit. *)
        val diff = take ((x ++ not_y) ++ (singleton ONE), nx)
        (* Delete tailing ZEROs. *)
        fun copy (ZERO, ZERO) = ZERO
          | copy (ZERO, ONE) = ONE
          | copy (ONE, _) = ONE
        val n = 
            length (filter (fn bit => bit = ZERO) (scani copy ZERO (rev diff)))
        val result = take (diff, nx - n)
      in
        (* If y = 0, then x; otherwise, x shouldn't be 0 (according to the 
           writeup, x >= y), then do above math *)
        if ny = 0 then x else result
      end

  val sub = op--
end
