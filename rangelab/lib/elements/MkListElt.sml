functor MkListElt (structure Elt : ELEMENT) =
struct
  structure Hashing = Elt.Hashing
  exception NYI

  val default = []
  type t = int list
  val equal : (int list * int list) -> bool = op=
  fun compare _ = raise NYI
  local
    open Hashing
    open Hashing.Word
    fun folder (e, hg) =
      combine (Elt.hashgen e, hg)
  in
  val hashgen = List.foldl folder (salt (fromInt 0))
  val hash = runHash o hashgen
  end
  fun toString xs =
      "[" ^ String.concatWith "," (List.map Int.toString xs) ^ "]"
end
