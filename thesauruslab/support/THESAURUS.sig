signature THESAURUS =
sig
  structure Seq : SEQUENCE

  type thesaurus

  (* make synonymSeq
   *   synonymSeq is a sequence of tuples (a : string, b : string seq)
   *   where the sequence b consists of the synonyms of a.
   *   constructs a thesaurus type for the input.
   *)
  val make : (string * string Seq.seq) Seq.seq -> thesaurus

  (* Returns the number of distinct words. *)
  val numWords : thesaurus -> int

  (* synonyms thesaurus word
   *   Returns the synonyms of a word in the thesaurus.
   *   Returns an empty sequence if the word is not in the thesaurus.
   *)
  val synonyms : thesaurus -> string -> string Seq.seq

  (* query thesaurus word1 word2
   *   Reports the shortest path from word1 to word2 as a sequence
   *   of strings with word1 first and word2 last.
   *   Returns the empty sequence if no path exists.
   *)
  val query : thesaurus -> string -> string -> string Seq.seq Seq.seq
end
