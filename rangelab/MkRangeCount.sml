functor MkRangeCount(structure OrdTable : ORD_TABLE) : RANGE_COUNT =
struct
  structure OrdTable = OrdTable
  open OrdTable

  (* Ordered table type: *)
  type 'a table = 'a table
  type 'a seq = 'a Seq.seq
  type point = Key.t * Key.t

  (* Use this to compare x- or y-values. *)
  val compareKey : (Key.t * Key.t) -> order = Key.compare

  (* Define this yourself *)
  type countTable = Key.t table table

  fun makeCountTable (S : point seq) : countTable =
      let
        (* Map input to singleton trees. 
           Then join them using scani, so we have an OrdTable 
           containing points on the left of each sweep line. *)
        fun pointToTree (x, y) = OrdTable.singleton(y, x)
        val yTreeSeq = Seq.map pointToTree S
        val bsts = Seq.scani OrdTable.join (OrdTable.empty()) yTreeSeq
        (* Organize all these OrdTables into a OrdTable*)
        fun bstToTree ((x, _), tree) = OrdTable.singleton(x, tree)
        val xTreeSeq = Seq.map2 bstToTree xSortedS bsts
      in
        Seq.reduce OrdTable.join (OrdTable.empty()) xTreeSeq
      end

  fun count (T : countTable)
                   ((xLeft, yHi) : point, (xRght, yLo) : point) : int  =
      let
        (* Whether left line is on point matters. But not for right line. *)
        val (size1, isOnPoint) = 
            case OrdTable.split(T, xLeft) of
              (_, SOME table1, _) => (size (getRange table1 (yLo, yHi)), 1)
            | (L, NONE, _) => 
                case last L of
                  NONE => (0, 0)
                | SOME (_, table1) => (size (getRange table1 (yLo, yHi)), 0)
        val size2 = 
            case OrdTable.split(T, xRght) of 
              (_, SOME table2, _) => size (getRange table2 (yLo, yHi))
            | (L, NONE, _) => 
                case last L of
                  NONE => 0
                | SOME (_, table2) => size (getRange table2 (yLo, yHi))
      in
        size2 - size1 + isOnPoint
      end
end
