functor MkBSTOrderedTable (structure Tree : BSTREE
                           structure Seq : SEQUENCE) : ORD_TABLE =
struct

  structure Table = MkBSTTable(structure Tree = Tree
                               structure Seq = Seq)

  (* Include all the functionalities of the standard Table *)
  open Table

  (* This is defined after "open" so it doesn't get overwritten *)
  structure Key = Tree.Key
  type key = Key.t

  fun first (T : 'a table) : (key * 'a) option =
      case (Tree.expose T) of
           NONE => NONE
         | SOME {left=L, key=k, value=v, right=R} =>
             case (Tree.expose L) of 
                   NONE => SOME (k, v) 
                 | _ => first(L)

  fun last (T : 'a table) : (key * 'a) option =
      case (Tree.expose T) of
        NONE => NONE
      | SOME {left, key=k, value=v, right=R} =>
          case (Tree.expose R) of
            NONE => SOME (k, v)
          | _ => last(R)

  fun previous (T : 'a table) (k : key) : (key * 'a) option =
      let
        val (L, _, _) = Tree.splitAt(T, k)
      in
        last(L)
      end

  fun next (T : 'a table) (k : key) : (key * 'a) option =
      let
        val (_, _, R) = Tree.splitAt(T, k)
      in
        first(R)
      end

  fun join (L : 'a table, R : 'a table) : 'a table =
      Tree.join (L, R)

  fun split (T : 'a table, k : key) : 'a table * 'a option * 'a table =
      Tree.splitAt (T, k)

  fun getRange (T : 'a table) (low : key, high : key) : 'a table =
      let
        val right =
            case Tree.splitAt (T, low) of
              (_, NONE, R) => R
            | (_, v, R) => Tree.join (Tree.singleton(low, Option.valOf v), R)
        val left = 
            case Tree.splitAt (right, high) of
              (L, NONE, _) => L
            | (L, v, _) => Tree.join (L, Tree.singleton(high, Option.valOf v))
      in
        left
      end
end
