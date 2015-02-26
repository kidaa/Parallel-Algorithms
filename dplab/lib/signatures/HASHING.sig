signature HASHING =
sig
  structure Word : WORD

  type hashgen
  val salt : Word.word -> hashgen
  val combine : (hashgen * hashgen) -> hashgen
  val runHash : hashgen -> int
end
