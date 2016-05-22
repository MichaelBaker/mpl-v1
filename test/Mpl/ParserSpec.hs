module Mpl.ParserSpec where

import Test.Hspec

import Mpl.Parser (AST(..), parse)

test name string result = it name $ do
  let (parses, _) = parse string
  parses `shouldBe` [result]

spec :: Spec
spec = do
  describe "grammar" $ do
    describe "integers" $ do
      test "a single digit"  "1"         (AInt "1")
      test "a single number" "12"        (AInt "12")
      test "long integer"    "987654321" (AInt "987654321")

    describe "float" $ do
      test "leading zero" "0.0"                   (AFloat "0.0")
      test "all digits"   "1234567890.0987654321" (AFloat "1234567890.0987654321")

    describe "lists" $ do
      test "empty list"                       "[]"                         (AList [])
      test "list with an int"                 "[12]"                       (AList [AInt "12"])
      test "list with two ints"               "[12 13]"                    (AList [AInt "12", AInt "13"])
      test "list with leading whitespace"     "[ 12 13]"                   (AList [AInt "12", AInt "13"])
      test "list with surrounding whitespace" " [ 12 13]"                  (AList [AInt "12", AInt "13"])
      test "crazy list"                       " [   1,53, 23, 8 12,13  ,]" (AList [AInt "1", AInt "53", AInt "23", AInt "8", AInt "12", AInt "13"])
