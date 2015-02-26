structure Tester :
sig
  val removeSeamsFile : string * string * int -> unit
  val removeSeamsDisplay : string * int -> unit
end
=
struct
  structure Seq = ArraySequence
  open Seq

  structure SeamFind = MkSeamFind(structure Seq = Seq) : SEAMFIND

  type pixel = { r : real, g : real, b : real }
  type image = { width : int, height : int, data : pixel seq seq }
  type gradient = real

  fun generateGradients {width, height, data} : gradient seq seq =
      let
        fun sq (v : real) = v * v
        fun d ({r=r1, g=g1, b=b1}, {r=r2, g=g2, b=b2}) =
            sq (r2 - r1) + sq (g2 - g1) + sq (b2 - b1)
        fun p (i, j) = nth (nth data i) j
        fun gradient i j =
            if j = width - 1 then Real.posInf
            else if i = height - 1 then 0.0
            else let
              val dx = d (p (i, j), p (i, j+1))
              val dy = d (p (i, j), p (i+1, j))
            in Math.sqrt (dx + dy)
            end
      in tabulate (fn i => tabulate (gradient i) width) height
      end

  fun removeSeam (img as {width, height, data} : image, seam) =
      let
         fun deleteIth(S,i) =
           Seq.tabulate (fn j => Seq.nth S (if (j < i) then j else j+1))
                        (width-1)
         val newData = Seq.map2 deleteIth data seam
      in
         {width=width-1, height=height, data = newData}
      end

  (* Removes k seams from an image *)
  fun removeSeams (img, k) =
      if (k <= 0) then img
      else removeSeams (removeSeam (img, SeamFind.findSeam
                                         (generateGradients img)), k-1)

  fun removeSeamsFile (inFile, outFile, k) =
      let
        val imageIn = ImageIO.fromFile(inFile)
        val imageOut = removeSeams(imageIn, k)
      in
        ImageIO.toFile(outFile, imageOut)
      end

  fun removeSeamsDisplay (inFile, k) =
      let
        val imageIn = ImageIO.fromFile(inFile)
        val imageOut = removeSeams(imageIn, k)
      in
        ImageIO.displayImage(imageOut)
      end
end
