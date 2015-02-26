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
      	(* For each j in the i the row, get the element with min cost
      	   from M[i-1][j-1], M[i-1][j] and M[i-1][j+1]
      		 Consider edge cases to avoid range exception. 
      		 So...nasty. Any simple way to do this??? *)
      	fun findMin (row, index) =
      		  if index = 0 then
      		  	let
      		  		val (l0, c0) = nth row 0
      		  		val (l1, c1) = nth row 1
      		  	in
      		  		if c0 <= c1 then (l0, c0)
      		  		else (l1, c1)
      		  	end
      		  else if index = n - 1 then
      		  	let
      		  		val (l0, c0) = nth row (index - 1)
      		  		val (l1, c1) = nth row index
      		  	in
      		  		if c0 <= c1 then (l0, c0)
      		  		else (l1, c1)
      		  	end
      		  else
      		  	let
      		  		val (l0, c0) = nth row (index - 1)
      		  		val (l1, c1) = nth row index
      		  		val (l2, c2) = nth row (index + 1)
      		  	in
      		  		if c0 <= c1 andalso c0 <= c2 then (l0, c0)
      		  		else if c1 <= c0 andalso c1 <= c2 then (l1, c1)
      		  		else (l2, c2)
      		  	end
      	(* Use M[i-1] and G[i] to generate new status M[i] *)
      	fun extendSeams (rowM, rowG) =
      			let
      				fun getM (i, g) = 
      					let
      						val (l, c) = findMin(rowM, i)
      					in
      						(i::l, c + g) (* Cost of list is O(1) *)
      					end
      			in
      				mapIdx getM rowG
      			end
      	(* Start from first row, iter all rows in G to get all seams *)
      	val base = tabulate (fn _ => ([], 0.0)) n
      	val seams = iter extendSeams base G
      	(* Select the seam with least cost *)
      	fun cmp ((l1, c1), (l2, c2)) =
      			if (c1 <= c2) then (l1, c1) else (l2, c2)
      	val (seam, _) = reduce cmp ([], Real.maxFinite) seams
      in
      	rev (fromList seam)
      end
end
