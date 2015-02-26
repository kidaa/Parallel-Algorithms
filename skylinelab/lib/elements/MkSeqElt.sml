functor MkSeqElt (structure Elt : ELEMENT
                  structure Seq : SEQUENCE) : ELEMENT =
struct
  structure Hashing = Elt.Hashing
  type t = Elt.t Seq.seq

  val default : t = Seq.empty ()
  val equal = Seq.equal Elt.equal
  val compare = Seq.collate Elt.compare
  fun toString s = Seq.toString Elt.toString s

  local
    open Hashing
    open Hashing.Word
    fun folder (hg, e) = combine (hg, Elt.hashgen e)
  in
  val hashgen = Seq.iter folder (salt (fromInt 0))
  val hash = runHash o hashgen
  end
end
