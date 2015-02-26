functor MkBigNumAdd(structure U : BIGNUM_UTIL) : BIGNUM_ADD =
struct
  structure Util = U
  open Util
  open Seq

  (* Remove this line when you're done. *)
  exception NotYetImplemented
  exception CopyNotCorrect

  infix 6 ++

  fun x ++ y =
      let
		(* Expand the shorter number; fill in with ZEROs. *)
      	val nx = length x
      	val ny = length y
      	val matched = 
      	    if nx > ny then map (fn i => (i, ZERO)) (drop (x, ny))
      	    else if nx < ny then map (fn i => (ZERO, i)) (drop (y, nx))
      	         else empty()
      	val zipped = append ((zip x y), matched)
        (* Get the sequence of carries *)
        fun get_carry_option (ONE, ONE) = SOME ONE 
          | get_carry_option (ZERO, ZERO) = SOME ZERO
          | get_carry_option ((ZERO, ONE) | (ONE, ZERO)) = NONE
        val carry_option1 = map get_carry_option zipped
        (* Fill in the "ripple" carries. *)
        fun copy (bit1, NONE) = bit1
          | copy (bit1, bit2) = bit2
        val (carry_option2,last) = scan copy (SOME ZERO) carry_option1
        fun get_carry (SOME bit) = bit
          | get_carry NONE = raise CopyNotCorrect
        val carry = map get_carry carry_option2
        (* Sum up all two numbers and carry-ins *)
        fun bit_xor ((ONE, ONE) | (ZERO, ZERO)) = ZERO
          | bit_xor ((ONE, ZERO) | (ZERO, ONE)) = ONE
        fun get_sum (carry_bit, (x_bit, y_bit)) =
        	bit_xor (carry_bit, bit_xor (x_bit, y_bit))
        val sum = map2 get_sum carry zipped
        (* See if need to add the last carry bit. *)
        val result = 
            if last = SOME ONE then append (sum, singleton ONE)
            else sum
      in
      	(* Check for special cases. *)
      	if nx = 0 andalso ny = 0 then empty()
      	else result
      end

  val add = op++
end
