functor MkRefThesaurusASP (ASP : ALL_SHORTEST_PATHS where type vertex = string)
  : THESAURUS =
struct
  structure Seq = ASP.Seq
  open Seq

  (* Remove the following two lines when you're done! *)
  exception NYI
  type nyi = unit

  (* You must define the following type and
   * explain your decision here with a comment.
   *)
  type thesaurus = (string * string seq) seq

  (* Task 3.1 *)
  fun make (S : (string * string seq) seq) : thesaurus =
    S

  (* Task 3.2 *)
  fun numWords (T : thesaurus) : int =
    let
      val flat = flatten (map (fn (x,S) => append (S, singleton x)) T)
      val sorted = sort String.compare flat
      fun filterer (ind, el) =
        (ind = 0) orelse (nth sorted (ind - 1) <> el)
    in
      length (filterIdx filterer sorted)
    end

  fun synonyms (T : thesaurus) (w : string) : string seq =
    let
      val matches = filter (fn (el,_) => el = w) T
    in
      case length matches of
        0 => empty ()
      | _ => #2 (nth matches 0)
    end

  (* Task 3.3 *)
  fun query (T : thesaurus) (w1 : string) (w2 : string) : string seq seq =
    raise NYI

end
