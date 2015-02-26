signature PARTIALSEQUENCE =
sig
  include BARESEQUENCE

  val rev : 'a seq -> 'a seq
  val append : 'a seq * 'a seq -> 'a seq
  val flatten : 'a seq seq -> 'a seq
  
  val map : ('a -> 'b) -> 'a seq -> 'b seq
  val filter : ('a -> bool) -> 'a seq -> 'a seq

  val enum : 'a seq -> (int * 'a) seq
  val mapIdx : ((int * 'a) -> 'b) -> 'a seq -> 'b seq

  val take : 'a seq * int -> 'a seq
  val drop : 'a seq * int -> 'a seq
  val showl : 'a seq -> 'a listview
  val showt : 'a seq -> 'a treeview

  val iter : ('b * 'a -> 'b) -> 'b -> 'a seq -> 'b
  val iterh : ('b * 'a -> 'b) -> 'b -> 'a seq -> 'b seq * 'b
  val reduce : ('a * 'a -> 'a) -> 'a -> 'a seq -> 'a

  val toList : 'a seq -> 'a list
end
