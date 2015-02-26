structure BoolElt : ELEMENT =
struct
  structure Hashing = Hashing
  type t = Bool.bool

  exception NYI

  val default = true
  val equal : t * t -> bool = op=
  fun compare _ = raise NYI
  local
    open Hashing
  in
  fun hashgen true = salt 0wx1
    | hashgen false = salt 0wx0
  val hash = runHash o hashgen
  end
  val toString = Bool.toString
end
