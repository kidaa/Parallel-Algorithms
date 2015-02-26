(**
 * 210lib/TreeSequenceTT.sml
 *
 * Implements SEQUENCE with 2-3 trees
 *)
structure TreeSequenceTT : SEQUENCE =
struct
  open Primitives
  exception NYI

  type 'a ord = 'a * 'a -> order

  datatype 'a tree = T0 | T1 of 'a | T2 of int * int * 'a tree * 'a tree 
                   | T3 of int * int * 'a tree * 'a tree * 'a tree
  type 'a seq = 'a tree
  datatype 'a treeview = EMPTY | ELT of 'a | NODE of 'a seq * 'a seq
  datatype 'a listview = NIL | CONS of 'a * 'a seq

  datatype 'a rtype = ONE of 'a tree | TWO of 'a tree * 'a tree

  exception Range
  exception Size

  fun depth S =
     case S of 
       T0 => 0
     | T1(_) => 1
     | T2(_,d,_,_) => d
     | T3(_,d,_,_,_) => d

  fun length S = 
     case S of 
       T0 => 0
     | T1(_) => 1
     | T2(n,_,_,_) => n
     | T3(n,_,_,_,_) => n

  fun empty _ = T0
  fun singleton x = T1(x)

  fun mT2(L,R) = T2(length L + length R, depth L + 1, L, R)
  fun mT3(L,M,R) = T3(length L + length M + length R , depth L + 1, L, M, R)

  fun makeNode2(L,R) =
     case (L,R) of 
         (ONE(l),ONE(r)) => ONE(mT2(l,r))
       | (TWO(l1,l2),ONE(r)) => ONE(mT3(l1,l2,r))
       | (ONE(l),TWO(r1,r2)) => ONE(mT3(l,r1,r2))

  fun makeNode3(L,M,R) =
     case (L,R) of 
         (ONE(l),ONE(r)) => ONE(mT3(l,M,r))
       | (TWO(l1,l2),ONE(r)) => TWO(mT2(l1,l2),mT2(M,r))
       | (ONE(l),TWO(r1,r2)) => TWO(mT2(l,M),mT2(r1,r2))

  fun appendp (s, t) = 
     case (s,t) of 
       (T0,_) => ONE(t)
     | (_,T0) => ONE(s)
     | _      => 
       let 
          val ds = depth s
          val dt = depth t
       in 
          if (ds > dt) then
            case s of T2(_,_,Ls,Rs) => makeNode2(ONE(Ls),appendp(Rs,t))
                    | T3(_,_,Ls,Ms,Rs) => 
                          makeNode3(ONE(Ls),Ms,appendp(Rs,t))
          else if (dt > ds) then
            case t of T2(_,_,Lt,Rt) => makeNode2(appendp(s,Lt),ONE(Rt))
                    | T3(_,_,Lt,Mt,Rt) => 
                          makeNode3(appendp(s,Lt),Mt,ONE(Rt))
          else TWO(s,t)
       end

  fun append (s,t) =
     case appendp(s,t) of
         ONE(x) => x
       | TWO(L,R) => mT2(L,R)

  fun map f S =
     case S of
        T0 => T0
      | T1(v) => T1(f(v))
      | T2(s,h,L,R) => T2(s,h,map f L,map f R)
      | T3(s,h,L,M,R) => T3(s,h,map f L,map f M,map f R)

  fun tabulate f n =
    let
       fun tab(s,l) =
         if (l = 1) then singleton(f(s))
         else 
           let val half = l div 2
           in append(tab(s,half),tab(s+half,l-half))
           end
    in 
      if n < 0 then raise Size
      else if n = 0 then empty () 
      else tab(0,n)
    end

  fun nth S i =
     case S of
       T0 => raise Range
     | T1(x) => if (i=0) then x
                else raise Range
     | T2(_,_,L,R) =>
        let val l = length L
        in 
          if (i >= l) then nth R (i-l)
          else nth L i
	end
     | T3(_,_,L,M,R) =>
        let val l = length L
            val m = length M
        in 
          if (i < l) then nth L i
          else if (i < l+m) then nth M (i-l)
          else nth R (i-(l+m))
	end

  fun toString f s =
      "<" ^ String.concatWith "," (List.tabulate (length s, f o nth s)) ^ ">"

  fun fromList l =
     let
       fun flist(nil,_) = (T0,nil)
         | flist(a::r,n) =
         if (n=1) then (T1(a),r)
         else 
           let val h = n div 2
               val (SL,rl) = flist(a::r,h)
               val (SR,rr) = flist(rl,n-h)
           in (append(SL,SR),rr) end
        val (r,_) = flist(l,List.length l)
      in 
          r
      end

  val % = fromList

  fun cut S i =
    case S of
      T0 => (T0,T0)
    | T1(x) => if (i = 0) then (T0,S)
               else (S,T0)
    | T2(_,_,L,R) =>
        let val l = length L
        in 
          if (i = l) then (L,R)
          else if (i < l) then 
            let val (LL,LR) = cut L i
            in (LL,append(LR,R))
            end
          else 
            let val (RL,RR) = cut R (i-l)
            in (append(L,RL),RR)
            end
	end
    | T3(_,_,L,M,R) =>
        let val l = length L
            val m = length M
        in 
          if (i < l) then 
            let val (LL,LR) = cut L i
            in (LL,append(LR,mT2(M,R)))
            end
          else if (i = l) then (L,mT2(M,R))
          else if (i < l+m) then 
            let val (ML,MR) = cut M (i-l)
            in (append(L,ML),append(MR,R))
            end
	  else if (i = l+m) then (mT2(L,M),R)
          else 
            let val (RL,RR) = cut R (i-l-m)
            in (append(mT2(L,M),RL),RR)
            end
	end

  fun subseq S (i, len') =
      if len' < 0 then raise Size
      else if i < 0 orelse i+len' > (length S) then raise Range
      else 
        let
          val (_,R) = cut S i
          val (L,_) = cut R len'
        in L end

  fun take (s, n) = subseq s (0, n)
  fun drop (s, n) = subseq s (n, length s - n)

  fun showl T0 = NIL
    | showl s = CONS (nth s 0, drop (s, 1))

  fun showt s =
      case s 
        of T0 => EMPTY
         | T1(v) => ELT(v)
         | _ => 
             let val half = (length s) div 2
               val (L,R) = cut s half
             in NODE (L, R) end

  fun rev s =
      case s 
        of T0 => T0
         | T1(v) => T1(v)
         | T2(n,d,a,b) => T2(n,d,rev b, rev a)
         | T3(n,d,a,b,c) => T3(n,d,rev c, rev b, rev a)

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

  fun flatten ss = reduce append (empty()) ss

  fun filter p s = 
     case s of 
       T0 => T0
     | T1(v) => if p(v) then s else T0
     | T2(_,_,L,R) =>
         let
            val (L',R') = (filter p L, filter p R)
         in
            append(L', R')
         end
     | T3(_,_,L,M,R) =>
         let
            val (L',M',R') = (filter p L, filter p M, filter p R)
         in
            append(L', append(M',R'))
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

  fun inject idx s = raise NYI

  fun collect cmp s =
      let
        val n = length s
        val (ks, vs) = unzip (sort (fn ((x,_), (y,_)) => cmp (x,y)) s)

        fun dk (0, _) = true
          | dk (i, k) = cmp (nth ks (i-1), k) <> EQUAL

        val starts = map (fn (i, _) => i) (filter dk (enum ks))
        val lengths = map2 op- (drop (append (starts, %[n]), 1)) starts

        fun make (i, len) = (nth ks i, subseq vs (i, len))
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
