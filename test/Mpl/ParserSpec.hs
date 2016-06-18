module Mpl.ParserSpec where

import Test.Hspec
import ASTHelpers (aparen, asquare, aint, asym)

import Mpl.AST    (AST(..))
import Mpl.Parser (parse)

test name string result = it name $ do
  let (parses, _) = parse string
  parses `shouldBe` [result]

spec :: Spec
spec = do
  describe "integer" $ do
    test "a single digit"  "1"         (aint 1)
    test "a single number" "12"        (aint 12)
    test "long integer"    "987654321" (aint 987654321)

  describe "identifier" $ do
    test "single letter"       "a"                (asym "a")
    test "camel cased word"    "uUadkjADkdfsjljD" (asym "uUadkjADkdfsjljD")
    test "symbolic identifier" "<$>"              (asym "<$>")
    test "octothorp"           "#"                (asym "#")

  describe "s-expression" $ do
    test "ident and int"   "(# 5)"             (aparen [asym "#", aint 5])
    test "idents and ints" "(1 <?> int 5)"     (aparen [aint 1, asym "<?>", asym "int", aint 5])
    test "nested"          "((a [int (b)]) 5)" (aparen [aparen [asym "a", asquare [asym "int", aparen [asym "b"]]], aint 5])
