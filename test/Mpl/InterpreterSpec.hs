module Mpl.InterpreterSpec where

import Test.Hspec

import Mpl.Interpreter (interpret)

test name string result = it name $ interpret string `shouldBe` result

spec :: Spec
spec = do
  describe "interpreter" $ do
    test "a single int"                "8907" "8907"
    test "a single float"              "234.522" "234.522"
    test "a list of ints and floats"   "  [ 123,234.5423, 2 3 6 234.322] \n\n" "[123, 234.5423, 2, 3, 6, 234.322]"
    test "an int -> int map"           "{1 2 3 4}" "{1: 2, 3: 4}"
    test "a float -> int map"          "{1.0 23 0.3 134}" "{0.3: 134, 1.0: 23}"
    test "a string -> float/int map"   "{\"hello\": 1.0, \"there\" : 1234,}" "{\"hello\": 1.0, \"there\": 1234}"
    test "simple function application" "(#([a] a) 5)" "5"
    test "simple closure"              "(#([a] (#([_] a) 1)) 5)" "5"
    test "deep closure"                "(#([a] (#([_] (#([_] (#([_] (#([_] a) 1)) 2)) 3)) 4)) 5)" "5"
    test "construct pair"              "(#([a b] [a b]) 1 2)" "[1, 2]"
    test "construct list of maps"      "(#([a b c] [{\"a\": a}, {b: b}, {c: \"3\"}]) 1 2 3)" "[{\"a\": 1}, {2: 2}, {3: \"3\"}]"
