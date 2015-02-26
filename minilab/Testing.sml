structure PS = MkPartialSequence (structure BareSeq = BareArraySequence)
structure SF = MkSeqFun (structure PartSeq = PS)

structure Tester =
struct
  (* You can either write your own tests here or play with your code
   *   in the SML/NJ REPL. We've opened PS and SF here to give you easy access
   *   to your code *)
  open PS
  open SF

  (* You might find these definitions useful *)
  val intSeqToStr =
    toString Int.toString

  fun seqEq cmp (S, T) =
    let
      fun checkPair i =
        if (i >= length S) orelse (i >= length T) then true else
        (cmp (nth S i, nth T i) = EQUAL) andalso (checkPair (i+1))
    in
      (length S = length T) andalso (checkPair 0)
    end

  val % = fromList

  (* ============ Here's an example: testing your map function ============ *)
  fun testMap () =
    let
      fun testOne (func, input, expected, desc) =
        let
          val result = map func input
          val correct = seqEq Int.compare (result, expected)
          val corrStr = if correct then "PASSED " else "FAILED "
        in
          print (corrStr ^ "(" ^ desc ^ " " ^ (intSeqToStr input) ^ ")" ^ "\n");

          if correct then () else
          (print ("  EXPECTED " ^ (intSeqToStr expected) ^ "\n");
           print ("  ACTUAL   " ^ (intSeqToStr result) ^ "\n"))
        end

      val tests : ((int -> int) * int seq * int seq * string) list = [
        (* put tests here:
         * (mapping function, input seq, expected output, description string) *)
        (fn x => x+1, %[1,2,3], %[2,3,4], "map (fn x => x+1)")
      ]
    in
      List.map testOne tests;
      ()
    end

end