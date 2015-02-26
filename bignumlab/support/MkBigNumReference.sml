functor MkBigNumAddRef(structure U : BIGNUM_UTIL) : BIGNUM_ADD =
struct
  structure Util = U
  open Util

  fun add (bn1, bn2) =
    fromIntInf ((toIntInf bn1) + (toIntInf bn2))
end

functor MkBigNumSubtractRef(structure U : BIGNUM_UTIL) : BIGNUM_SUBTRACT =
struct
  structure Util = U
  open Util

  fun sub (bn1, bn2) =
    fromIntInf ((toIntInf bn1) - (toIntInf bn2))
end

functor MkBigNumMultiplyRef(structure U : BIGNUM_UTIL) : BIGNUM_MULTIPLY =
struct
  structure Util = U
  open Util

  fun mul (bn1, bn2) =
    fromIntInf ((toIntInf bn1) * (toIntInf bn2))
end
