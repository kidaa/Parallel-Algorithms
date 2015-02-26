functor MkThesaurusUtils (Seq : SEQUENCE) : sig
  structure Seq : SEQUENCE

  (* Given a filename, returns the contents of the file as a string. *)
  val readFile : string -> string

  (* Parses a thesaurus file format into a sequence of (w,S) pairs
   * such that every word w is paired with its sequence of synonyms S.
   *)
  val parseString : string -> (string * (string Seq.seq)) Seq.seq

  (* Composition of the above two *)
  val fromFile : string -> (string * (string Seq.seq)) Seq.seq

end =
struct

  (* Invalid format error *)
  exception BadFile

  structure Seq = Seq
  open Seq

  fun readFile filename = TextIO.inputAll (TextIO.openIn filename)

  fun parseString thesaurusFile =
      let
        fun isNewline c = (c = #"\n")
        fun isSpace c = (c = #" ")

        (* split into lines *)
        val lines = fromList (String.tokens isNewline thesaurusFile)

        (* separates a string (line) into tokens, and then pairs
         * up the first token with the rest
         *)
        fun process line =
            let
              val words = fromList (String.tokens isSpace line)
              val w0 = nth words 0
              val rest = drop (words,1)
            in
              (w0, rest)
            end
      in
        map process lines
      end

  val fromFile = parseString o readFile
end
