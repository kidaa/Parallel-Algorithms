functor MkBabble
  (structure Tok : TOKEN
   structure Mkvc : MARKOV_CHAIN where type Key.t = Tok.token Tok.Seq.seq
   sharing Tok.Seq = Mkvc.Seq)
  : BABBLE =
struct
  structure Seq = Tok.Seq
  structure Rand = Mkvc.Rand
  structure Tok = Tok
  structure Mkvc = Mkvc

  exception NYI

  fun chainFromCorpus toks k =
      let
        (* Given a location i, generate two tokens that satisfy k-gram *)
        fun generatePair i =
            (Seq.subseq toks (i, k), Seq.subseq toks (i + 1, k))
        val pairSeq = Seq.tabulate generatePair ((Seq.length toks) - k)
      in
        Mkvc.build pairSeq
      end
      
  fun babble C n r =
      let
        (* Append two tokens. Since there is overlap, it is the same as
         * append the last word of second token to the former one. 
         *)
        fun tokAppend (tokSeq1, tokSeq2) =
            let
              val len2 = Seq.length tokSeq2
              val tok = 
                  Seq.singleton (Seq.nth tokSeq2 (len2 - 1))
            in
              Seq.append (tokSeq1, tok)
            end
      in
        Mkvc.iter tokAppend (Seq.empty()) n C r
      end
    
end