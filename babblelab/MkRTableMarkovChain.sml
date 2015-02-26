functor MkRTableMarkovChain
  (structure RTable : RTABLE)
  : MARKOV_CHAIN =
struct
  structure RTable = RTable
  structure Rand = RTable.Rand
  structure Seq = RTable.Seq
  structure Key = RTable.Key

  type chain = (Key.t Seq.seq) RTable.table

  exception NYI
  exception KeyDoesNotExist

  open RTable

  fun build S =
      collect S

  fun merge (C1, C2) =
      RTable.merge Seq.append (C1, C2)

  fun shrink C =
      let
        (* Calculate the probabilities to all the states from an entry of 
         * the chain. 
         *)
        fun getProbability S =
            let 
              val n = Seq.length S
              (* Count the number of occurence of each key in S. 0 doesn't 
               * mean anything here. 
               *)
              val T = RTable.collect (Seq.map (fn x => (x, 0)) S)
            in
              (* Calculate probabilities from T and length of S. *)
              RTable.map (fn x => (real)x / (real)n) (RTable.map Seq.length T)
            end
      in
        RTable.map getProbability C
      end

  fun iter f b n C r =
      (* Deal with special cases first. *)
      if n = 0 orelse RTable.size C = 0 then b
      else
        let
          (* Randomly choose a start state. Since special case size C = 0 has
           * been excluded above, the exception should never be raised.
           *)
          val start = 
              case rselect C r of
                   SOME t => t
                 | NONE => raise KeyDoesNotExist
          (* Find next key in the path as well as the output. *)
          fun findNext (s, key, seed, count) =
              let
                (* Get the sequence of next key. If this is an empty sequence,
                 * there is no out going path, process should end despite the
                 * value of n. 
                 *)
                val seq = 
                    case RTable.find C key of
                         SOME sequence => sequence
                       | NONE => Seq.empty()
              in
                if count = 0 orelse Seq.length seq = 0 then s
                else
                  (* If there is further steps that can be taken, update
                   * parameters and find next. *)
                  let
                    val (x, newSeed) = 
                        Rand.randomInt seed (SOME (0, Seq.length seq))
                    val newKey = Seq.nth seq x
                    val newS = f (s, newKey)
                  in
                    findNext (newS, newKey, newSeed, count - 1)
                  end
              end   
        in
          findNext (f(b, start), start, r, n - 1)
        end
end
