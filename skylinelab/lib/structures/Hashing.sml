(* Implementation of the incrementally constructive hash algorithm, MurmurHash
 *   This means that we can use it for structural hashing of data structures
 *   (hashing a sequence by incrementally hashing all of its values)
 *
 * Talk to Naman for more information.
 *
 * NOTE TO STUDENTS: This structure is used only internally. *)
structure Hashing : HASHING =
struct
  open LargeWord

  (* Hashes will be 31-bit ints *)
  type hash = int
  (* 32-bit hash intermediate *)
  type hashstate = word
  (* function which transforms one hashstate into another *)
  type hashgen = hashstate -> hashstate

  val wordSz = Word.fromInt wordSize

  (* Left bitwise rotate *)
  fun rotateL (i : word, n : Word.word) : word =
    orb (<< (i, n), >> (i, Word.- (wordSz, n)))

  local
    val c1 : word = 0wxcc9e2d51
    val c2 : word = 0wx1b873593
    val r1 : Word.word = 0wxf
    val r2 : Word.word = 0wxd
    val m : word = 0wx5
    val n : word = 0wxe6546b64
  in
  fun mix (b0: word) (h0: hashstate) : hashstate =
    let
      val b1 = b0 * c1
      val b2 = rotateL (b1, r1)
      val b3 = b2 * c2
      val h1 = xorb (h0, b3)
      val h2 = rotateL (h1, r2)
      val h3 = h2 * m + n
    in
      h3
    end
  end

  local
    val c1 : word = 0wx85ebca6b
    val c2 : word = 0wxc2b2ae35
    val r1 : Word.word = 0wx10
    val r2 : Word.word = 0wxd
    val r3 : Word.word = r1
  in
  fun finalize (h0: hashstate) : hash =
    let
      val h1 = xorb (h0, << (h0, r1))
      val h2 = h1 * c1
      val h3 = xorb (h2, << (h2, r2))
      val h4 = h3 * c2
      val h5 = xorb (h4, << (h4, r3))
    in
      (* Because sml/nj has 31-bit ints, we drop the highest order bit from the
       * word before converting back to an int. *)
      Word.toIntX (Word.fromLargeWord h5)
    end
  end

  val salt : word -> hashgen = mix

  fun combine (f : hashgen, g : hashgen) : hashgen = (g o f)

  local
    val defaultSeed = fromInt 294967291
  in
  fun runHash (hg : hashgen) = finalize (hg defaultSeed)
  end

  structure Word = LargeWord

end
