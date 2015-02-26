structure StringElt : ELEMENT = 
struct
  structure Hashing = Hashing
  type t = String.string

  val default = ""
  val equal : t * t -> bool = op=
  val compare = String.compare
  local
    open Hashing
    open Hashing.Word
  in
  fun hashgen str =
    let
      val chL = String.explode str
      fun folder (ch, hg) = combine (salt (fromInt (Char.ord ch)), hg)
    in
      List.foldl folder (salt 0wx0) chL
    end
  val hash = runHash o hashgen
  end
  fun toString s = "\"" ^ String.toString s ^ "\""
end
