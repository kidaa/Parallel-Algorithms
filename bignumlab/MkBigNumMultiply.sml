functor MkBigNumMultiply(structure BNA : BIGNUM_ADD
                         structure BNS : BIGNUM_SUBTRACT
                         sharing BNA.Util = BNS.Util) : BIGNUM_MULTIPLY =
struct
  structure Util = BNA.Util
  open Util
  open Seq
  open Primitives

  (* Remove this line when you're done. *)
  exception NotYetImplemented

  infix 6 ++ --
  infix 7 **

  fun x ++ y = BNA.add (x, y)
  fun x -- y = BNS.sub (x, y)
  fun x ** y =
      let
        (* Zip and extend the numbers to the same length. *)
        fun zip_extend a b = 
            let
              val na = length a
              val nb = length b
              val extend = 
                  if na > nb then map (fn i => (i, ZERO)) (drop (a, nb))
                  else if na < nb then map (fn i => (ZERO, i)) (drop (b, na))
                       else empty()
              val zipped = zip a b
            in
              append (zipped, extend)
            end
        fun bit_and ((ZERO, ZERO) | (ZERO, ONE) | (ONE, ZERO)) = ZERO
          | bit_and (ONE, ONE) = ONE
        fun shift (num, n) =
            append ((tabulate (fn i => ZERO) n), num)
        (* Recursive funcion. *)
        fun multiply num_pair = 
            case showt num_pair of 
              EMPTY => empty()
            | ELT (x_bit, y_bit) => singleton (bit_and (x_bit, y_bit))
            | NODE (L, R) => 
              let
                (* Applying the method described in writeup. *)
                val nl = length L
                val nr = length R
                val p = map (fn (a, b) => a) R
                val r = map (fn (a, b) => b) R
                val q = map (fn (a, b) => a) L
                val s = map (fn (a, b) => b) L
                val M = zip_extend (p ++ q) (r ++ s)
                val (p_r, q_s, pq_rs) = 
                    par3((fn () => multiply R), (fn () => multiply L),
                         (fn () => multiply M))
              in
                q_s ++ shift (pq_rs -- p_r -- q_s, nl) ++ shift (p_r, 2 * nl)
              end
      in
        (* Delete zeros. *)
        let
          val result_with_zeros = multiply (zip_extend x y)
          fun copy (ZERO, ZERO) = ZERO
            | copy (ZERO, ONE) = ONE
            | copy (ONE, _) = ONE
          val n = 
              length (filter (fn bit => bit = ZERO) 
                             (scani copy ZERO (rev result_with_zeros)))
          val result = take (result_with_zeros, (length result_with_zeros) - n)
          val nx = length x
          val ny = length y
        in
          (* Some special cases. *)
          case (nx, ny) of
               (0, _) => empty()
             | (_, 0) => empty()
             | (1, _) => y
             | (_, 1) => x
             | (_, _) => result
        end
      end

  val mul = op**
end
