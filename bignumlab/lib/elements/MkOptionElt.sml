functor MkOptionElt (structure Elt : ELEMENT) : ELEMENT =
struct
  structure Hashing = Elt.Hashing
  type t = Elt.t option

  val default = NONE

  fun equal (NONE, NONE) = true
    | equal (SOME x, SOME y) = Elt.equal (x, y)
    | equal _ = false

  fun compare (NONE, NONE) = EQUAL
    | compare (NONE, SOME _) = LESS
    | compare (SOME _, NONE) = GREATER
    | compare (SOME x, SOME y) = Elt.compare (x, y)

  local
    open Hashing
    open Word
  in
  fun hashgen NONE = salt (fromInt 0)
    | hashgen (SOME v) = Elt.hashgen v

  val hash = runHash o hashgen
  end

  fun toString NONE = "NONE"
    | toString (SOME x) = "SOME (" ^ Elt.toString x ^ ")"
end
