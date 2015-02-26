functor MkSeamFind(structure Seq : SEQUENCE) : SEAMFIND =
struct
  structure Seq = Seq
  open Seq

  type pixel = { r : real, g : real, b : real }
  type image = { width : int, height : int, data : pixel seq seq }
  type gradient = real

  fun findSeam G =
      let
      	val m = length G
      	val n = length (nth G 0)
      	fun findSeam' ((rowM, seamIndices, range), rowG) =
      		let
      			(* Use G(i, j) and M(i-1, [j-1, j+1]) to generate M(i, j) 
      				 Consider edge cases to avoid range exception. *)
	      		fun getMinCost (j, g) =
			      		if j = 0 
			      		then g + Real.min(nth rowM 0, nth rowM 1)
			      		else if j = n - 1 
			      			   then g + Real.min (nth rowM (j-1), nth rowM j)
			      			   else
			      			   	 let
			      		   	 	   val temp = Real.min(nth rowM (j-1), nth rowM j)
			      		   	   in
			      		   	 	   g + Real.min(temp, nth rowM (j+1))
			      		   	   end
			      val newRowM = mapIdx getMinCost rowG
			      (* Choose column index for this row *)
			      fun cmp (a, b) = (* inversed cmp to find min using argmax *)
			      		if nth newRowM a < nth newRowM b then GREATER
			      		else if nth newRowM a > nth newRowM b then LESS
			      			   else EQUAL
			      val thisColIdx = nth range (argmax cmp range)\
			      val newSeamIndices = thisColIdx :: seamIndices
			      val newRange = 
			      		if thisColIdx = 0 then %[0, 1]
			      		else if thisColIdx = n - 1 then %[n - 2, n - 1]
			      			   else %[thisColIdx - 1, thisColIdx, thisColIdx + 1]
      		in
      			(newRowM, newSeamIndices, newRange)
      		end
      	val base = (tabulate (fn _ => 0.0) n, [], tabulate (fn i => i) n)

      	val (_, indicesList, _) = iter findSeam' base G
      in
      	rev (fromList indicesList)
      end
end
