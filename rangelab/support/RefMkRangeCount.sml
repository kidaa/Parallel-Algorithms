functor RefMkRangeCount(structure OrdTable : ORD_TABLE) : RANGE_COUNT =
struct
  structure OrdTable = OrdTable
  open OrdTable

  (* Ordered table type: *)
  type 'a table = 'a table
  type 'a seq = 'a Seq.seq
  type point = Key.t * Key.t

  fun kLtEq (x,y) =
    case Key.compare (x,y) of
      GREATER => false
    | _ => true

  (* Use this to compare x- or y-values. *)
  val compareKey : (Key.t * Key.t) -> order = Key.compare

  (* Define this yourself *)
  type countTable = point seq

  fun makeCountTable (S : point seq) : countTable =
    S

  fun count (T : countTable)
            ((xLeft, yHi) : point, (xRght, yLo) : point) : int  =
    let
      fun filt (x,y) = kLtEq (xLeft, x) andalso
                       kLtEq (x, xRght) andalso
                       kLtEq (yLo, y) andalso
                       kLtEq (y, yHi)
    in
      Seq.length (Seq.filter filt T)
    end
end
