 functor MkBSTRTable
  (structure Tree : BSTREE
   structure Rand : RANDOM210)
  : RTABLE =
struct
  structure Rand = Rand

  structure Table = MkBSTTable(structure Tree = Tree
                               structure Seq = Rand.Seq)

  (* Include all the functionalities of the standard Table *)
  open Table

  (* type 'a table = 'a Tree.bst *)

  (* given a seed, returns an arbitrary key from the table *)
  fun rselect t seed =
    if size t = 0 then NONE else
    let
      fun rselect' t k =
        case Tree.expose t of
          NONE => NONE
        | SOME {left, key, right, ...} =>
            if k < size left then rselect' left k else
            if k = size left then (SOME key) else
            rselect' right (k - size left - 1)

      val (k, _) = Rand.randomInt seed (SOME (0, size t))
    in
      rselect' t k
    end
end