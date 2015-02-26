functor MkPairElt (structure EltA : ELEMENT
                   structure EltB : ELEMENT
                   sharing EltA.Hashing = EltB.Hashing) : ELEMENT =
struct
  structure Hashing = EltA.Hashing
  type t = EltA.t * EltB.t

  val default = (EltA.default, EltB.default)

  fun equal ((xa,xb),(ya,yb)) =
      EltA.equal (xa,ya) andalso EltB.equal (xb,yb)

  fun compare ((xa,xb),(ya,yb)) =
      case EltA.compare (xa,ya)
        of EQUAL => EltB.compare (xb,yb)
         | ord => ord

  local
    open Hashing
  in
  fun hashgen (a,b) =
    combine (EltA.hashgen a, EltB.hashgen b)
  val hash = runHash o hashgen
  end

  fun toString (a,b) =
      "(" ^ EltA.toString a ^ "," ^ EltB.toString b ^ ")"
end
