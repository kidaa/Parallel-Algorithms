signature RANGE_COUNT =
sig
  structure OrdTable : ORD_TABLE

  type point = (OrdTable.Key.t * OrdTable.Key.t)
  type countTable

  val makeCountTable : point OrdTable.Seq.seq -> countTable
  val count : countTable -> (point * point) -> int
end

