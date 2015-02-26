functor RefMkBSTOrderedTable (structure Tree : BSTREE
                           structure Seq : SEQUENCE) : ORD_TABLE =
struct

  structure Table = MkBSTTable(structure Tree = Tree
                               structure Seq = Seq)

  (* Include all the functionalities of the standard Table *)
  open Table

  (* This is defined after "open" so it doesn't get overwritten *)
  structure Key = Tree.Key
  type key = Key.t

  (* Remember, type 'a table = 'a Tree.bst *)

  fun maxcmp ((k1 : key, _ : 'a), (k2 : key, _ : 'a)) = Key.compare (k1, k2)
  fun mincmp ((k1 : key, _ : 'a), (k2 : key, _ : 'a)) = Key.compare (k2, k1)

  fun kEq (x,y) =
    case Key.compare (x,y) of
      EQUAL => true
    | _ => false
  fun kGt (x,y) =
    case Key.compare (x,y) of
      GREATER => true
    | _ => false
  fun kLt (x,y) =
    case Key.compare (x,y) of
      LESS => true
    | _ => false

  fun first (T : 'a table) : (key * 'a) option =
    let
      val S = toSeq T
    in
      SOME (Seq.nth S (Seq.argmax mincmp S))
        handle Seq.Range => NONE
    end

  fun last (T : 'a table) : (key * 'a) option =
    let
      val S = toSeq T
    in
      SOME (Seq.nth S (Seq.argmax maxcmp S))
        handle Seq.Range => NONE
    end

  fun previous (T : 'a table) (k : key) : (key * 'a) option =
    let
      val S = Seq.filter (fn (x,_) => kLt(x,k)) (toSeq T)
    in
      SOME (Seq.nth S (Seq.argmax maxcmp S))
        handle Seq.Range => NONE
    end

  fun next (T : 'a table) (k : key) : (key * 'a) option =
    let
      val S = Seq.filter (fn (x,_) => kGt(x,k)) (toSeq T)
    in
      SOME (Seq.nth S (Seq.argmax mincmp S))
        handle Seq.Range => NONE
    end

  fun join (L : 'a table, R : 'a table) : 'a table =
    fromSeq (Seq.append (toSeq L, toSeq R))

  fun split (T : 'a table, k : key) : 'a table * 'a option * 'a table =
    let
      val S : (key * 'a) Seq.seq = toSeq T
      val m = SOME (#2 (Seq.nth (Seq.filter (fn (x,_) => kEq(x,k)) S) 0))
                handle Range => NONE
      val L = Seq.filter (fn (x,_) => kLt(x,k)) S
      val R = Seq.filter (fn (x,_) => kGt(x,k)) S
    in
      (fromSeq L, m, fromSeq R)
    end

  fun getRange (T : 'a table) (low : key, high : key) : 'a table =
    let
      fun inRange (x : key, _ : 'a) =
        (kEq(low,x) orelse kLt(low,x)) andalso
        (kEq(x,high) orelse kLt(x,high))
    in
      fromSeq (Seq.filter inRange (toSeq T))
    end
end
