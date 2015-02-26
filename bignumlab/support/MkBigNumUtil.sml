functor MkBigNumUtil(structure S : SEQUENCE) : BIGNUM_UTIL =
struct
  structure Seq = S
  open Seq

  (* A bignum x is a bit sequence where x_0 is the least significant
   * bit. We adopt the convention that if x represents the number 0,
   * x is a empty sequence. Furthermore, if x > 0, the right-most bit
   * of x must be ONE (i.e., there cannot be trailing zeros at the end)
   *)
  datatype bit = ZERO | ONE
  type bignum = bit seq

  exception Negative

  (* Raise 2^z *)
  fun pow2 (z : Int.int) : IntInf.int = IntInf.<< (1, Word.fromInt z)

  (* Converts an arbitrarily long positive integer to a bit sequence *)
  fun fromIntInf (x : IntInf.int) : bignum =
      let
        (* Use math to calculate the ith bit of x *)
        fun ith i =
            case IntInf.andb (IntInf.div (x, pow2 (i)), 1)
              of 1 => ONE
               | _ => ZERO
      in
        case IntInf.compare (x, 0)
          of LESS => (print "Error: converting negative number to bignum\n";
                      raise Negative)
           | EQUAL => empty ()
           | GREATER => tabulate ith (IntInf.log2(x) + 1)
      end

  fun toIntInf (s : bignum) : IntInf.int =
      if length s = 0 then 0
      else let
        (* Get the ith bit of s and convert it into an integer *)
        fun ith i = IntInf.* (pow2 i, (fn ZERO => 0 | ONE => 1) (nth s i))
      in
        reduce IntInf.+ 0 (tabulate ith (length s))
      end
end
