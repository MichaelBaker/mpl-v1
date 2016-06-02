module Mpl.CompilerSpec where

import Test.Hspec

import Mpl.Compiler (compile)

test name string result = it name $ compile string `shouldBe` result

-- TODO: Update these
spec :: Spec
spec = do
  return ()
  -- describe "interpreter" $ do
  --   test "unit"                        "()" "()"
  --   test "a single int"                "8907" "8907"
  --   test "a single float"              "234.522" "234.522"
  --   test "a list of ints and floats"   "  [ 123,234.5423, 2 3 6 234.322] \n\n" "[123, 234.5423, 2, 3, 6, 234.322]"
  --   test "an int -> int map"           "{1 2 3 4}" "{1: 2, 3: 4}"
  --   test "a float -> int map"          "{1.0 23 0.3 134}" "{1.0: 23, 0.3: 134}"
  --   test "a string -> float/int map"   "{\"hello\": 1.0, \"there\" : 1234,}" "{\"hello\": 1.0, \"there\": 1234}"
  --   test "forcing a thunk"             "((# [] 5))" "5"
  --   test "simple function application" "((# [a int] a) 5)" "5"
  --   test "simple closure"              "((# [a int] ((# [_ int] a) 1)) 5)" "5"
  --   test "deep closure"                "((# [a int] ((# [_ int] ((# [_ int] ((# [_ int] ((# [_ int] a) 1)) 2)) 3)) 4)) 5)" "5"
  --   test "construct pair"              "((# [a int b int] [a b]) 1 2)" "[1, 2]"
  --   test "construct list of maps"      "((# [a int b int c int] [{\"a\": a}, {b: b}, {c: \"3\"}]) 1 2 3)" "[{\"a\": 1}, {2: 2}, {3: \"3\"}]"
  --   test "partial application"         "((# [a int b int] [a b]) 1)" "#([b] [a, b])"
  --   test "curried application"         "(((# [a int b int] [a b]) 1) 2)" "[1, 2]"
