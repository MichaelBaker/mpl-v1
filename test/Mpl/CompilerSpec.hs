module Mpl.CompilerSpec where

import Test.Hspec

import Mpl.Compiler (compile)

test name string result = it name $ compile string `shouldBe` result

spec :: Spec
spec = do
  describe "interpreter" $ do
    test "a single int"                "8907" "8907"
    test "forcing a thunk"             "((# [] 5))" "5"
    test "simple function application" "((# [a] a) 5)" "5"
    test "simple closure"              "((# [a] ((# [_] a) 1)) 5)" "5"
    test "deep closure"                "((# [a] ((# [_] ((# [_] ((# [_] ((# [_] a) 1)) 2)) 3)) 4)) 5)" "5"
    test "multiple parameters"         "((# [a b] a) 1 2)" "1"
    test "partial application"         "((# [a b] a) 1)" "(# [b] a)"
