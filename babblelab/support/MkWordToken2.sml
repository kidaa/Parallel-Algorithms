functor MkWordToken2
  (structure Seq : SEQUENCE) 
  : TOKEN = 
struct
  structure Seq = Seq
  structure Elt = StringElt

  type token = Elt.t

  fun tokenize corpus =
    let
      val delim = not o Char.isAlphaNum
    in
      Seq.fromList (String.tokens delim corpus)
    end

  val compare = String.compare

  fun toString tok = tok

  fun toStringConcat toks = 
    String.concatWith " " (Seq.toList toks)
end