structure ImageIO :
sig
  structure Seq : SEQUENCE
  type 'a seq = 'a Seq.seq

  type pixel = { r : int, g : int, b : int }
  type image = { width : int, height : int, data : pixel seq seq }

  val fromFile: string -> image
  val toFile: string * image -> unit
  val displayImage: image -> unit

  val auxpath: string ref
end = struct
  structure Seq = ArraySequence
  open Seq

  val auxpath = ref "./support/"

  type pixel = { r : int, g : int, b : int }
  type image = { width : int, height : int, data : pixel seq seq }

  local
    structure V = Word8Vector

    fun one(s,i) = Word8.toInt(V.sub(s,i));
    fun two(s,i) = one(s,i)+256*one(s,i+1);
    fun four(s,i)= two(s,i)+256*256*two(s,i+2);

    fun mk1 v =  Word8.fromInt (v mod 256)
    fun mk2 v = V.fromList[mk1 v,
                           mk1 (v div 256)];
    fun mk4 v = V.concat[mk2 (v mod 65536),
                         mk2 (v div 65536)];
  in

  fun getPixel ({data, ...}:image) (x, y) = nth (nth data y) x

  fun fromRawFile fileName =
      let
        val ins = TextIO.openIn fileName
        val w = valOf (Int.fromString (valOf (TextIO.inputLine ins)))
        val h = valOf (Int.fromString (valOf (TextIO.inputLine ins)))

        fun getPix _ =
          let
            val s = valOf (TextIO.inputLine ins)
            val L = List.map (valOf o Int.fromString) (String.tokens Char.isSpace s)
          in
            {r = List.nth (L, 0), g = List.nth (L, 1), b = List.nth (L, 2)}
          end

        val data = tabulate (fn _ => tabulate getPix w) h
        val _ = TextIO.closeIn ins
      in
        {width=w, height=h, data=data}
      end

  val userID = case OS.Process.getEnv "USER"
                 of SOME user => user
                  | NONE => "noname"

  val magicTag = userID ^ "-" ^ (Time.toString (Time.now()))
  val rawIn = "/tmp/seamIn" ^ magicTag ^ ".raw"
  val rawOut = "/tmp/seamOut" ^ magicTag ^ ".raw"
  val jpgOut = "/tmp/seamOut" ^ magicTag ^ ".jpg"

  fun fromFile fileName =
      let val _ = OS.Process.system
        (!auxpath ^ "/image-feed.py " ^ fileName ^ " > " ^ rawIn)
      in fromRawFile rawIn
      end

  fun toRawFile (fileName, {width=w, height=h, data=data}:image) =
      let
        val ous = TextIO.openOut fileName
        val _ = TextIO.output (ous, Int.toString w)
        val _ = TextIO.output (ous, "\n")
        val _ = TextIO.output (ous, Int.toString h)

        fun printPix {r, g, b} =
          let
            val _ = TextIO.output (ous, "\n")
            val _ = TextIO.output (ous, Int.toString r ^ " ")
            val _ = TextIO.output (ous, Int.toString g ^ " ")
            val _ = TextIO.output (ous, Int.toString b)
          in
            ()
          end
        val _ = map (fn r => map printPix r) data
      in
        TextIO.closeOut ous
      end

  fun toFile (fileName, img) =
      let
        val _ = toRawFile(rawOut, img)
        val _ = OS.Process.system
          (!auxpath ^ "/image-rewrite.py " ^ fileName ^ " < " ^ rawOut)
      in ()
      end

  fun displayFile fileName = OS.Process.system("image " ^ fileName ^ " &")

  fun displayImage img = (toFile(jpgOut, img); displayFile(jpgOut); ())

  end
end
