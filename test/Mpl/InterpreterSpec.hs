module Mpl.InterpreterSpec where

import Test.Hspec

import Mpl.Interpreter (eval)

test name string result = it name $ eval string `shouldBe` result

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
