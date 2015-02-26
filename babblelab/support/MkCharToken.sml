functor MkCharToken
  (structure Seq : SEQUENCE) 
  : TOKEN =
struct
  structure Seq = Seq
  (* I would make a CharElt structure, but Hashing structure huh?? *)
  structure Elt = StringElt

  type token = Elt.t

  fun tokenize corpus =
    ((Seq.map (fn c => String.implode [c])) o 
     (Seq.filter (fn c => Char.isPrint c orelse c = #"\n")))
    (Seq.fromList (String.explode corpus))

  val compare = String.compare
  
  fun toString tok = tok

  fun toStringConcat toks =
    String.concat (Seq.toList toks)
end