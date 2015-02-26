functor MkThesaurusASP (ASP : ALL_SHORTEST_PATHS where type vertex = string)
  : THESAURUS =
struct
  structure Seq = ASP.Seq
  open Seq
  open ASP

  (* Remove the following two lines when you're done! *)
  exception NotYetImplemented
  type nyi = unit

  (* You must define the following type *)
  type thesaurus = graph

  fun make (S : (string * string seq) seq) : thesaurus =
      let 
        fun decompose (w, synonymsSeq) =
            Seq.map (fn synonyms => (w, synonyms)) synonymsSeq
        val base = Seq.empty()
      in
        ASP.makeGraph (Seq.reduce Seq.append base (Seq.map decompose S))
      end

  fun numWords (T : thesaurus) : int =
      ASP.numVertices T

  fun synonyms (T : thesaurus) (w : string) : string seq =
      ASP.outNeighbors T w

  fun query (T : thesaurus) (w1 : string) =
      let
        val asp = makeASP T w1
      in
        fn w => report asp w
      end
end
