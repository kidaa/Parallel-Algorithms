functor MkWordToken1 
  (structure Seq : SEQUENCE) 
  : TOKEN = 
struct
  structure Seq = Seq
  structure Elt = StringElt

  type token = Elt.t

  fun tokenize corpus =
    let
      fun either f g x = (f x) orelse (g x)
      val delim = either Char.isSpace (not o Char.isPrint)
    in
      Seq.fromList (String.tokens delim corpus)
    end

  val compare = String.compare

  fun toString tok = tok

  fun toStringConcat toks = 
    String.concatWith " " (Seq.toList toks)
end