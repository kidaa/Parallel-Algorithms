functor MkSeqFun
  (structure PartSeq : PARTIALSEQUENCE)
  : SEQFUN =
struct
  open PartSeq

  (* Remove this line when you're done *)
  exception NotYetImplemented

  fun allPerms S =
      let
        val n = length S
      in
        (* If this is an empty sequence, return empty sequence. If there is 
         * only one element, return it. If there are two elements, two perms.
         * If there are n > 2 elements, take the first element, insert it to
         * all possible locations to get all perms. Do this recursively. 
         *)
        case n of
             0 => empty()
           | 1 => singleton (singleton (nth S 0))
           | 2 => let
                    val x = singleton (nth S 0)
                    val y = singleton (nth S 1)
                  in
                    append(singleton (append (x,y)), singleton(append (y,x)))
                  end
           | _ => let
                    val element = nth S 0
                    val seq = subseq S (1,(n-1))
                    fun insert_all s =
                        let
                          fun insert loc =
                            case loc of
                               0 => append ((singleton element), s)
                             | _ => let
                                      val s1 = take (s,loc)
                                      val s2 = singleton element
                                      val s3 = drop (s, loc)
                                    in
                                      append (append(s1, s2), s3)
                                    end
                        in
                          tabulate insert n
                        end
                  in
                    flatten (map insert_all (allPerms seq))
                  end
      end

  fun permSort cmp S =
      let
        val perms = allPerms S
        val n = length perms
        fun compare perm =
            case length perm of
                 0 => 1
               | 1 => 1
               | _ => let
                        val s = drop (perm, 1)
                      in
                        case cmp((nth perm 0), (nth s 0)) of
                             GREATER => 0
                           | _ => compare s
                      end
        fun isInOrder perm =
            case compare perm of
                 1 => true
               | _ => false
      in
        if n = 0 then empty() else nth (filter isInOrder perms) 0
      end
end
