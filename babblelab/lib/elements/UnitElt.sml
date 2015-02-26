structure UnitElt : ELEMENT =
struct
  structure Hashing = Hashing
  type t = unit

  exception NYI

  val default = ()
  fun equal ((),()) = true
  fun compare ((),()) = raise NYI
  local
    open Hashing
  in
  fun hashgen () = salt 0wx0
  val hash = runHash o hashgen
  end
  fun toString () = "()"
end
