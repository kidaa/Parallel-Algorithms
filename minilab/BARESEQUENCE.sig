signature BARESEQUENCE =
sig
  type 'a seq

  exception Range
  exception Size

  datatype 'a listview = NIL | CONS of 'a * 'a seq
  datatype 'a treeview = EMPTY | ELT of 'a | NODE of 'a seq * 'a seq

  val nth : 'a seq -> int -> 'a
  val length : 'a seq -> int
  val toString : ('a -> string) -> 'a seq -> string

  val empty : unit -> 'a seq
  val singleton : 'a -> 'a seq
  val tabulate : (int -> 'a) -> int -> 'a seq
  val fromList : 'a list -> 'a seq

  val subseq : 'a seq -> int * int -> 'a seq
end
