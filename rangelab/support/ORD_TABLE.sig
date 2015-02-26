(* Signature for the Ordered Table abstract data type *)
signature ORD_TABLE =
sig

(* Include the entire TABLE signature, but with Key ascribing to ORDKEY *)
  
  structure Key : ORDKEY
  structure Seq : SEQUENCE
  structure Set : SET where Key = Key and Seq = Seq

  type 'a table
  type 'a seq = 'a Seq.seq
  type key = Key.t
  type set = Set.t

  val size : 'a table -> int
  val domain : 'a table -> set
  val range : 'a table -> 'a seq
  val toString : ('a -> string) -> 'a table -> string
  val toSeq : 'a table -> (key * 'a) seq

  val find : 'a table -> key -> 'a option
  val insert : ('a * 'a -> 'a) -> (key * 'a) -> 'a table -> 'a table
  val delete : key -> 'a table -> 'a table

  val empty : unit -> 'a table
  val singleton : key * 'a -> 'a table
  val tabulate : (key -> 'a) -> set -> 'a table
  val collect : (key * 'a) seq -> 'a seq table
  val fromSeq : (key * 'a) seq -> 'a table

  val map     : ('a -> 'b) -> 'a table -> 'b table
  val mapk    : (key * 'a -> 'b) -> 'a table -> 'b table
  val filter  : ('a -> bool) -> 'a table -> 'a table
  val filterk : (key * 'a -> bool) -> 'a table -> 'a table

  val iter   : ('b * (key * 'a) -> 'b) -> 'b -> 'a table -> 'b
  val iterh  : ('b * (key * 'a) -> 'b) -> 'b -> 'a table -> ('b table * 'b)
  val reduce : ('a * 'a -> 'a) -> 'a -> 'a table -> 'a

  val merge   : ('a * 'a -> 'a) -> ('a table * 'a table) -> 'a table
  val extract : 'a table * set -> 'a table
  val erase   : 'a table * set -> 'a table

  val $ : (key * 'a) -> 'a table


(* Additional ordered table operations depending on the key's ordering: *)

 (* Given an ordered table T, first evaluates to SOME(k,v) if 
    (k,v) is in T and k is the minimum key in T. Otherwise, it evaluates
    to NONE. *)
  val first : 'a table -> (key * 'a) option

  (* Given an ordered table T, last evaluates to SOME(k,v) if 
     (k,v) is in T and k is the maximum key in T. Otherwise, it
     evaluates to NONE. *)
  val last : 'a table ->  (key * 'a) option

  (* Given an ordered table T and a key k, previous evaluates to
    SOME(k',v') if (k',v') is in T and k' is the largest key in T less
    than k. Otherwise, it evaluates to NONE. *)
  val previous : 'a table -> key -> (key * 'a) option

  (* Given an ordered table T and a key k, next evaluates to
     SOME(k',v') if (k',v') is in T and k' is the smallest key in T greater
     than k. Otherwise, it evaluates to NONE. *)
  val next : 'a table -> key -> (key * 'a) option

  (* Given an ordered table T and a key k, split evaluates to a triple
     consisting of 
     1) an ordered table containing all (k',v) in T such that k' < k, 
     2) SOME(v) if (k,v) is in T and NONE otherwise, and 
     3) an ordered table containing all (k',v) in T such that k' > k *)
  val split : 'a table * key -> 'a table * 'a option * 'a table

  (* Given two ordered tables T1 and T2, where all the keys in T1 are
     less than all the keys in T2, join evaluates to an ordered table
     that is the union of T1 and T2.*)
  val join : 'a table * 'a table -> 'a table
  
  (* Given a table T and keys lo and hi, getRange evaluates to an ordered
     table of all (k,v) in T such that lo <= k <= hi *)
  val getRange : 'a table -> key * key ->'a table 
end
