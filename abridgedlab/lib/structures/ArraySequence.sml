(**
 * 210lib/ArraySequence.sml
 *
 * Implements SEQUENCE with
 *   type 'a seq = 'a ArraySlice.slice
 *)
structure ArraySequence : SEQUENCE =
struct
  open Primitives

  structure A = Array
  structure AS = ArraySlice

  type 'a seq = 'a AS.slice
  type 'a ord = 'a * 'a -> order

  datatype 'a treeview = EMPTY | ELT of 'a | NODE of 'a seq * 'a seq
  datatype 'a listview = NIL | CONS of 'a * 'a seq

  exception Range
  exception Size

  val length : 'a seq -> int = AS.length
  fun empty _ = AS.full (A.fromList [])
  fun singleton x = AS.full (A.fromList [x])

  fun tabulate f n =
      if n < 0 then raise Size else
      AS.full (A.tabulate (n, f))

  fun nth s i =
      AS.sub (s, i) handle Subscript => raise Range

  fun toString f s =
      "<" ^ String.concatWith "," (List.tabulate (length s, f o nth s)) ^ ">"

  fun fromList l = AS.full (A.fromList l)

  val % = fromList

  fun subseq s (i, len) =
      if len < 0 then raise Size else
      AS.subslice (s, i, SOME len) handle Subscript => raise Range

  fun take (s, n) = subseq s (0, n)
  fun drop (s, n) = subseq s (n, length s - n)

  fun showl s =
      case AS.getItem s of
        SOME (x, xs) => CONS (x, xs)
      | NONE => NIL

  fun showt s =
      case length s of
        0 => EMPTY
      | 1 => ELT (nth s 0)
      | n => let val half = n div 2 in
             NODE (take (s, half), drop (s, half))
             end

  fun rev s =
      let val n = length s
      in tabulate (fn i => nth s (n-1-i)) n
      end

  fun append (s, t) =
      let val (ns, nt) = (length s, length t)
        fun ith i = if i >= ns then nth t (i-ns) else nth s i
      in tabulate ith (ns+nt)
      end

  fun iterh f b s =
      let
        fun iterh' s (old, cur) =
            case showl s
              of NIL => (rev (fromList old), cur)
               | CONS (x, xs) => iterh' xs (cur::old, f (cur, x))
      in iterh' s ([], b)
      end

  fun iter f b s = #2 (iterh f b s)
  fun toList s = iter (fn (l,x) => x::l) [] (rev s)

  fun merge cmp s t =
      let
        (* Sequential merge. Pretend it's parallel! *)
        fun merge' [] t = t
          | merge' s [] = s
          | merge' (x::xs) (y::ys) =
            if cmp (y, x) = LESS
            then y::merge' (x::xs) ys
            else x::merge' xs (y::ys)

      in fromList (merge' (toList s) (toList t))
      end

  fun sort cmp s =
      case showt s
        of EMPTY => s
         | ELT x => singleton x
         | NODE (l, r) =>
           let val (l',r') = par (fn()=>sort cmp l, fn()=>sort cmp r)
           in merge cmp l' r'
           end

  fun enum s = tabulate (fn i => (i, nth s i)) (length s)
  fun map f s = tabulate (f o (nth s)) (length s)
  fun mapIdx f = map f o enum

  fun map2 f s t =
      tabulate (fn i => f (nth s i, nth t i)) (Int.min (length s, length t))

  fun unmap2 (spl : 'a -> 'b * 'c) s =
      let
        val n = length s
        val s' = map spl s
      in (tabulate (#1 o nth s') n, tabulate (#2 o nth s') n)
      end

  fun zip s t = map2 (fn x => x) s t
  fun unzip s = unmap2 (fn x => x) s

  fun reduce f b s =
      case length s
        of 0 => b
         | 1 => f (b, nth s 0)
         | n => let
                  fun contract i =
                      if i = n div 2 then nth s (2*i)
                      else f (nth s (2*i), nth s (2*i+1))
                in reduce f b (tabulate contract ((n+1) div 2))
                end

  (* scan (1) combines base case at bottom of recursion *)
  fun scan f b s =
      case length s
        of 0 => (empty (), b)
         | 1 => (singleton b, f (b, nth s 0))
         | n =>
           let
             fun contract i =
                 if i = n div 2 then nth s (2*i)
                 else f (nth s (2*i), nth s (2*i+1))
             val s' = tabulate contract ((n+1) div 2)
             val (r, res) = scan f b s'
             fun expand i =
                 if i mod 2 = 0 then nth r (i div 2)
                 else f (nth r (i div 2), nth s (i-1))
           in (tabulate expand n, res)
           end

  local
    fun scan' f S =
        if length S = 1 then (empty (), nth S 0)
        else let
          val n = length S
          fun contract i =
              if i = n div 2 then nth S (2*i)
              else f (nth S (2*i), nth S (2*i + 1))
          val S' = tabulate contract ((n+1) div 2)
          val (R, res) = scan' f S'
          fun expand 0 = nth S 0
            | expand i =
              if i mod 2 = 1 then nth R (i div 2)
              else f (nth R ((i-1) div 2), nth S i)
        in (tabulate expand (n-1), res)
        end
  in
    (* scan (2) combines base case after recursion *)
    fun scan f b S =
        if length S = 0 then (empty (), b)
        else let
          val (R, res) = scan' f S
          val R' = map (fn x => f (b, x)) R
        in (append (singleton b, R'), f (b, res))
        end
  end

  fun scani f b s =
      let val (r, res) = scan f b s
      in drop (append (r, singleton res), 1)
      end

  fun flatten ss =
      let
        val (starts, n) = scan op+ 0 (map length ss)
        val res = tabulate (fn _ => NONE) n
        fun write i (j,x) = AS.update (res, i+j, SOME x)
        val _ = map (fn (i,s) => mapIdx (write i) s) (zip starts ss)
      in map valOf res
      end

  fun filter p s =
      if length s = 0 then s else
      let
        val opts = map (fn e => if p e then SOME e else NONE) s
        val incrs = map (fn SOME _ => 1 | NONE => 0) opts
        val (psums, sz) = scan (op+) 0 incrs

        val res = AS.full (A.array (length s, nth s 0))
        fun update (SOME e, i) = AS.update (res, i, e)
          | update (NONE, _) = ()
      in
        AS.app update (zip opts psums);
        AS.subslice (res, 0, SOME sz)
      end

  fun filterIdx p =
      map (fn (_, x) => x) o (filter p) o enum

  fun equal cmp (s1,s2) =
      length s1 = length s2 andalso
      reduce (fn (x,y) => x andalso y) true (map2 cmp s1 s2)

  fun argmax cmp s =
      if length s = 0 then raise Range
      else let
        fun best (i, j) =
            if cmp (nth s j, nth s i) = GREATER then j else i
      in reduce best 0 (tabulate (fn i => i) (length s))
      end

  fun inject idx s =
      let
        val res = tabulate (nth s) (length s)
        fun update (i,x) =
            AS.update (res, i, x) handle Subscript => raise Range
      in
        AS.app update idx;
        res
      end

  fun collect cmp s =
      let
        val n = length s
        val (ks, vs) = unzip (sort (fn ((x,_), (y,_)) => cmp (x,y)) s)

        fun dk (0, _) = true
          | dk (i, k) = cmp (nth ks (i-1), k) <> EQUAL

        val starts = map (fn (i, _) => i) (filter dk (enum ks))
        val lengths = map2 op- (drop (append (starts, %[n]), 1)) starts

        fun make (i, len) =
          if len < 0 then raise Size else
          (nth ks i, subseq vs (i, len))
      in map2 make starts lengths
      end

  fun collate cmp (s1, s2) =
      case (showl s1, showl s2)
        of (NIL, NIL) => EQUAL
         | (NIL, _) => LESS
         | (_, NIL) => GREATER
         | (CONS (x, xs), CONS (y, ys)) =>
           case cmp (x, y)
             of EQUAL => collate cmp (xs, ys)
              | ord => ord
end
