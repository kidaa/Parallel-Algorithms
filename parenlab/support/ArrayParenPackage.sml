(* Wrapper for ArraySequence that also provides the paren type *)
structure ArrayParenPackage : PAREN_PACKAGE =
struct
  structure Seq = ArraySequence
  datatype paren = OPAREN | CPAREN
end

(* Helper structure, only provided for this lab *)
structure Option210 =
struct
  (* If both arguments are SOME, apply f to them.
   * If both are NONE, NONE.
   * Otherwise, SOME. *)
  fun either _ (NONE, NONE) = NONE
    | either _ ((SOME x, NONE) | (NONE, SOME x)) = SOME x
    | either f (SOME x, SOME y) = SOME (f (x, y))

  (* If both arguments are SOME, apply f to them.
   * Otherwise, NONE. *)
  fun both f (SOME x, SOME y) = SOME (f (x, y))
    | both _ _ = NONE

  val intMin = either Int.min
  val intMax = either Int.max
  val realMin = either Real.min
  val realMax = either Real.max

  val intMinus = both (op -)
  val intPlus = both (op +)
end
