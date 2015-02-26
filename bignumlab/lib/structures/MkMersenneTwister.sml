functor MkMersenneTwister(structure Seq : SEQUENCE) : RANDOM210 =
struct
  structure Seq = ArraySequence
  structure Word = LargeWord

  type state = (int * Word.word Array.array)
  type rand = state

  val arrlen = 624
  val offset = 397

  (* Helper for imperative things *)
  fun loop i f =
    if   i <= 0
    then ()
    else (f (i - 1); loop (i - 1) f)

  (* Create a rand state from an int seed. *)
  local
    val magic : Word.word = 0wx6c078965
    open Array
  in
  fun fromInt (i : int) : rand =
    let
      val mt : Word.word array = array (arrlen, Word.fromInt i)

      (* Generate the state imperatively *)
      fun looper i = 
        let
          val prev = sub (mt, i)
          val shifted = Word.>> (prev, 0wx1e)
          val xored = Word.xorb (prev, shifted)
          val muled = Word.* (magic, xored)
          val added = Word.+ (muled, Word.fromInt i)
        in
          update (mt, i+1, added)
        end
        val _ = loop 623 looper
    in
      (0, mt)
    end
  end


  local
    val magic : Word.word = 0wx9908b0df
    open Array
  in
  fun generate orig =
    let
      (* Create a copy of the original state array *)
      val mt : Word.word array = array (arrlen, 0wx0)
      val _ = copy {src = orig, dst = mt, di = 0}

      (* Modify the new state *)
      fun looper i =
        let
          val topbit =
            Word.andb (sub (mt, i), 0wx80000000)
          val lowbits =
            Word.andb (sub (mt, (i + 1) mod arrlen), 0wx7fffffff)
          val y = Word.+ (topbit, lowbits)
          val maybe =
            Word.xorb (sub (mt, (i + offset) mod arrlen), Word.>> (y, 0wx1))
          val new =
            if   Word.mod (y, 0wx2) = 0wx0
            then maybe
            else Word.xorb (maybe, magic)
        in
          update (mt, i, new)
        end
      val _ = loop 624 looper
    in
      mt
    end
  end

  local
    val magic1 : Word.word = 0wx9d2c5680
    val magic2 : Word.word = 0wxefc60000
    open Array
  in
  fun randomWord (ind, orig) =
    let
      val mt = case ind of 0 => generate orig | _ => orig
      val y = sub (mt, ind)

      val y1 = Word.xorb (y, Word.>> (y, 0wxb))
      val y2 = Word.xorb (y1, Word.andb (Word.<< (y1, 0wx7), magic1))
      val y3 = Word.xorb (y2, Word.andb (Word.<< (y2, 0wxf), magic2))
      val y4 = Word.xorb (y3, Word.>> (y3, 0wx12))
    in
      (y4, ((ind + 1) mod arrlen, mt))
    end
  end


  fun randomInt rng range =
    let
      val (wd, rng') = randomWord rng
      val i = Word31.toIntX (Word31.fromLargeWord wd)
      val i' =
        case range of
            NONE => i
          | SOME (low, high) => (i mod (high - low)) + low
    in
      (i', rng')
    end

  fun randomReal rng range =
    let
      val maxInt = valOf Int.maxInt
      val (i, rng') = randomInt rng NONE
      val r = Real.fromInt (Int.abs i) / Real.fromInt maxInt
      val r' =
        case range of
            NONE => r
          | SOME (low, high) => (r * (high - low)) + low
    in
      (r', rng')
    end

  (* For these, I use lists for potential efficiency; using sequences
   * requires making several passes. *)

  fun randomIntSeq rng range len =
    let
      fun randomIntList rng range 0 = ([], rng)
        | randomIntList rng range len =
            if len < 0 then raise Seq.Size else
            let
              val (i, rng') = randomInt rng range
              val (L, rng'') = randomIntList rng' range (len - 1)
            in
              (i::L, rng'')
            end
      val (L, rng') = randomIntList rng range len
    in
      (Seq.% L, rng')
    end

  fun randomRealSeq rng range len =
    let
      fun randomRealList rng range 0 = ([], rng)
        | randomRealList rng range len =
            if len < 0 then raise Seq.Size else
            let
              val (r, rng') = randomReal rng range
              val (L, rng'') = randomRealList rng' range (len - 1)
            in
              (r::L, rng'')
            end
      val (L, rng') = randomRealList rng range len
    in
      (Seq.% L, rng')
    end

  fun flip rng len =
    randomIntSeq rng (SOME (0,2)) len

end
