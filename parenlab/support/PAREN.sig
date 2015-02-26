signature PAREN_PACKAGE =
sig
  structure Seq : SEQUENCE
  datatype paren = OPAREN | CPAREN
end

(* Discussed in recitation 1 *)
signature PAREN_MATCH =
sig
  structure P : PAREN_PACKAGE

  val parenMatch : P.paren P.Seq.seq -> bool
end

signature PAREN_DIST =
sig
  structure P : PAREN_PACKAGE

  val parenDist : P.paren P.Seq.seq -> int option
end
