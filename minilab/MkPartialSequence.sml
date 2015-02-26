functor MkPartialSequence
  (structure BareSeq : BARESEQUENCE)
  : PARTIALSEQUENCE =
struct
  open BareSeq

  (* Remove this line when you're done *)
  exception NotYetImplemented
  exception EmptyNode

  fun rev S =
      let
        val n = length S
      in
        tabulate (fn i => nth S (n-i-1)) n
      end

  fun map f S =
      let
        val n = length S
        fun func i = f (nth S i)
      in
        tabulate func n
      end

  fun enum S =
      let
        val n = length S
        fun func i = (i, (nth S i))
      in
        tabulate func n
      end

  fun mapIdx f S =
      tabulate (fn i => f (i, (nth S i))) (length S)

  fun append (S, T) =
      let
        val ns = length S
        val nt = length T
        fun func i =
            if (i < ns) then (nth S i) else (nth T (i-ns))
      in
        tabulate func (ns + nt)
    end


  fun take (S, n) =
      subseq S (0, n)

  fun drop (S, n) =
      subseq S (n, (length S) - n)

  fun showl S =
      let
        val n = length S
      in
        if n = 0 then NIL else CONS(nth S 0, subseq S (1,n-1))
      end

  fun showt S =
      let
        local
            fun divBy2 x =
                if x mod 2 = 0 then x div 2 else (x-1) div 2
        in
            val n = length S
            val k = divBy2 n
        end
      in
        case n of
             0 => EMPTY
           | 1 => ELT(nth S 0)
           | _ => NODE(subseq S (0, k),subseq S (k, n-k))
      end

  fun reduce f b S =
      let
        val n = length S
        fun rec_func (f,S) =
            case showt S of
                 EMPTY => raise EmptyNode
               | ELT(a) => a
               | NODE(left, right) => f (rec_func (f,left), rec_func (f,right))
      in
        if n = 0 then b else f (b, (rec_func (f,S)))
      end

  fun iterh f b S =
      let
        fun rec_func (f,T) =
            case (length T) of
                 1 => [f(b,(nth T 0)), b]
               | k => let
                        val x::xs = rec_func (f, (subseq T (0,(k - 1))))
                      in
                        (f(x, (nth T (k-1)))) :: (x :: xs)
                      end
      in
        if length S = 0 then ((empty()),b)
        else let
               val seq = rev (fromList(rec_func(f,S)))
               val n = length seq
             in
               ((take (seq,(n-1))),(nth seq (n-1)))
             end
      end

  fun iter f b S =
      let
        val n = length S
      in
        case n of
             0 => b
           | 1 => f (b,(nth S 0))
           | _ => f ((iter f b (subseq S (0, n-1))), (nth S (n-1)))
      end

  fun toList S =
      let
        val n = length S
      in
        if n = 0 then [] else
        (nth S 0) :: toList (subseq S (1,n-1))
      end

  fun flatten S =
      if length S = 0 then empty() else reduce append (empty()) S

  fun filter f S =
      if length S = 0 then empty()
      else let
             fun combine (b,s) =
                 if (f (nth s 0)) = true then (append (b, s)) else b
           in
             reduce combine (empty()) (map (fn x => singleton x) S)
           end
end 
