functor MkTripleElt (structure EltA : ELEMENT
                     structure EltB : ELEMENT
                     structure EltC : ELEMENT
                     sharing EltA.Hashing = EltB.Hashing
                     and EltB.Hashing = EltC.Hashing) : ELEMENT =
struct
  structure Hashing = EltA.Hashing
  type t = EltA.t * EltB.t * EltC.t

  exception NYI

  val default = (EltA.default, EltB.default, EltC.default)

  fun equal ((xa,xb,xc),(ya,yb,yc)) =
      EltA.equal(xa,ya) andalso EltB.equal(xb,yb) andalso EltC.equal(xc,yc)

  fun compare ((xa,xb,xc),(ya,yb,yc)) =
      case EltA.compare (xa,ya)
        of EQUAL => (case EltB.compare (xb,yb)
                       of EQUAL => EltC.compare (xc,yc)
                        | ordb => ordb)
         | orda => orda

  local
    open Hashing
  in
  fun hashgen (a,b,c) =
    combine (EltA.hashgen a, combine (EltB.hashgen b, EltC.hashgen c))
  val hash = runHash o hashgen
  end

  fun toString (a,b,c) = 
      "(" ^ EltA.toString a ^ "," ^
            EltB.toString b ^ "," ^
            EltC.toString c ^ ")"
end
