signature BIGNUM_UTIL =
sig
  structure Seq : SEQUENCE
  datatype bit = ZERO | ONE
  type bignum = bit Seq.seq

  exception Negative

  (* convert to and from SML/NJ's IntInf *)
  val fromIntInf : IntInf.int -> bignum
  val toIntInf : bignum -> IntInf.int
end

signature BIGNUM_ADD =
sig
  structure Util : BIGNUM_UTIL

  (* add (x, y) computes x + y. *)
  val add : Util.bignum * Util.bignum -> Util.bignum
end

signature BIGNUM_SUBTRACT =
sig
  structure Util : BIGNUM_UTIL

  (* sub (x, y) computes x - y, assuming x >= y *)
  val sub : Util.bignum * Util.bignum -> Util.bignum
end

signature BIGNUM_MULTIPLY =
sig
  structure Util : BIGNUM_UTIL

  (* mul (x, y) computes x * y. *)
  val mul : Util.bignum * Util.bignum -> Util.bignum
end

signature BIGNUM =
sig
  val add : IntInf.int * IntInf.int -> IntInf.int
  val sub : IntInf.int * IntInf.int -> IntInf.int
  val mul : IntInf.int * IntInf.int -> IntInf.int
end
