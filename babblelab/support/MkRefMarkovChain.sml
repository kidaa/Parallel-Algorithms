(* This is a "correct" but purposefully terrible implementation of markov
 * chains for use as a testing structure. You should not attempt to mimic the
 * techniques used in this file. *)
functor MkRefMarkovChain
  (structure RTable : RTABLE)
  : MARKOV_CHAIN =
struct
  structure Seq = RTable.Seq
  structure RTable = RTable
  structure Rand = RTable.Rand
  structure Key = RTable.Key

  type chain = (Key.t Seq.seq) RTable.table
  type dumbchain = (Key.t * Key.t list) list

  val fromDumbChain : dumbchain -> chain =
    (RTable.map Seq.fromList) o RTable.fromSeq o Seq.fromList

  val toDumbChain : chain -> dumbchain =
    Seq.toList o RTable.toSeq o (RTable.map Seq.toList)

  fun listFindMany (key : Key.t) (L : (Key.t * 'a) list) : 'a list =
    case L of
      []        => []
    | (k,v)::L' =>
        let val rest = listFindMany key L'
        in if Key.equal (key, k) then v :: rest else rest
        end

  fun listFind (key : Key.t) (L : (Key.t * 'a) list) : 'a option =
    case listFindMany key L of
      []   => NONE
    | v::_ => SOME v

  fun listContains (key : Key.t) (L : (Key.t * 'a) list) : bool =
    isSome (listFind key L)

  fun noDups (result : (Key.t * 'a) list, L : (Key.t * 'a) list) =
    case L of
      []        => result
    | (k,v)::L' => if listContains k result then noDups (result, L')
                   else noDups ((k,v)::result, L')

  fun bunchTogether (L : (Key.t * 'a) list) : (Key.t * 'a list) list =
    let
      val keys = List.map #1 (noDups ([], L))
    in
      List.map (fn k => (k, listFindMany k L)) keys
    end

  fun build (pairs : (Key.t * Key.t) Seq.seq) : chain =
    let
      val pairsList = Seq.toList pairs
    in
      fromDumbChain (bunchTogether pairsList)
    end

  fun merge (mkvc1, mkvc2) =
    let
      val (mkvc1', mkvc2') = (toDumbChain mkvc1, toDumbChain mkvc2)
      val together = mkvc1' @ mkvc2'
      val keys = List.map #1 (noDups ([], together))
      fun grabBoth k =
        case (listFind k mkvc1', listFind k mkvc2') of
          (NONE    , NONE    ) => raise Fail "RefMkvc.merge is broken"
        | (NONE    , SOME ks ) => (k, ks)
        | (SOME ks , NONE    ) => (k, ks)
        | (SOME ks1, SOME ks2) => (k, ks1 @ ks2)
    in
      fromDumbChain (List.map grabBoth keys)
    end

  fun shrink mkvc =
    let
      fun shrink' (L, T) =
        case L of
          [] => T
        | (k, ks) :: L' =>
            let
              fun weight us =
                (Real.fromInt (length us)) / (Real.fromInt (length ks))
              fun calcOne (k, us) =
                (k, weight us)
              val withUs = bunchTogether (List.map (fn k => (k, ())) ks)
              val weightSeq = Seq.fromList (List.map calcOne withUs)
              val kT = RTable.fromSeq weightSeq
            in
              shrink' (L', RTable.insert (fn (x,_) => x) (k, kT) T)
            end
    in
      shrink' (toDumbChain mkvc, RTable.empty ())
    end

  fun iter f b n C r =
    (* Not necessary to implement this for testing *)
    raise Fail "Not Implemented"

end