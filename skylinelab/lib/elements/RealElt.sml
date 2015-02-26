structure RealElt : ELEMENT =
struct
  structure Hashing = Hashing
  type t = Real.real

  val default = 0.0
  val equal = Real.==
  val compare = Real.compare
  val toString = Real.toString
  local
    open Hashing
    open Hashing.Word
    fun realToWords (r: real) : word * word =
      let
        val vec = Unsafe.blastWrite r
        (* Deal with endianness *)
        val ind =
          case Word8Vector.sub (vec, 0) of
              0wx33 => (fn i => Int.- (103, i))
            | _ (* 0wx00 *) => (fn i => Int.+ (i, 96))
        val bytes : Word8.word Vector.vector =
          Unsafe.cast (Vector.tabulate (8, fn i => Word8Vector.sub (vec, ind i)))
        fun ith i = Word8.toLargeWord (Vector.sub (bytes, i))

        val upperOrder = List.foldl orb 0wx0 [ith 0, << (ith 1, 0wx8), << (ith 2, 0wx10), << (ith 3, 0wx18)]
        val lowerOrder = List.foldl orb 0wx0 [ith 4, << (ith 5, 0wx8), << (ith 6, 0wx10), << (ith 7, 0wx18)]
      in
        (upperOrder, lowerOrder)
      end
  in
  fun hashgen r =
    let
      val (upper, lower) = realToWords r
    in
      combine (salt upper, salt lower)
    end
  val hash = runHash o hashgen
  end
end
