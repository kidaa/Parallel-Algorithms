structure Tests =
struct
  (* Add your test cases to this list: *)
  val tests = [
    "()",
    "()()",
    "(()))",
    "",
    "(",
    ")",
    "((((",
    "))))",
    ")(()())()",
    "(()(()(()())())())"
  ]
end
