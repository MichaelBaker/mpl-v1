module Mpl.ParserSpec where

import Test.Hspec
import ASTHelpers (aparen, asquare, acurly, aint, afloat, atext, asym, tagcurly, tagparens, tagsquare)

import Mpl.AST    (AST(..))
import Mpl.Parser (parse)

test name string result = it name $ do
  let (parses, _) = parse string
  parses `shouldBe` [result]

param name ty = aparen [asym ":", asym name, asym ty]

spec :: Spec
spec = do
  describe "integer" $ do
    test "a single digit"  "1"         (aint 1)
    test "a single number" "12"        (aint 12)
    test "long integer"    "987654321" (aint 987654321)

  describe "float" $ do
    test "leading zero" "0.0"                   (afloat 0.0)
    test "all digits"   "1234567890.0987654321" (afloat 1234567890.0987654321)

  describe "text" $ do
    test "empty text" "\"\""      (atext "")
    test "plain text" "\"hello\"" (atext "hello")

  describe "symbols" $ do
    test "single letter"       "a"                (asym "a")
    test "camel cased word"    "uUadkjADkdfsjljD" (asym "uUadkjADkdfsjljD")
    test "symbolic identifier" "<$>"              (asym "<$>")
    test "octothorp"           "#"                (asym "#")

  describe "s-expressions" $ do
    test "paren"                     "()"                         (aparen  [])
    test "curly"                     "{}"                         (acurly  [])
    test "square"                    "[]"                         (asquare [])
    test "square with one item"      "[12]"                       (asquare [aint 12])
    test "square with items"         "[12 13]"                    (asquare [aint 12, aint 13])
    test "square crazy"              " [   1 53  23  8 12 13   ]" (asquare [aint 1,  aint 53, aint 23, aint 8, aint 12, aint 13])

    test "simple parens"
      "(# [] 5)"
      (aparen [asym "#", asquare [], aint 5])

    test "complex parens"
      "(: [t] (# [(: a t)] 5))"
      (aparen [asym ":", asquare [asym "t"], (aparen [asym "#", asquare [param "a" "t"], aint 5])])

  describe "tagged s-expressions" $ do
    test "simple tagged curly sexp"
      "#{}"
      (tagcurly "#" [])

    test "tagged parens with items"
      "<$>(1 2 3)"
      (tagparens "<$>" [aint 1, aint 2, aint 3])

    test "tagged nested square brackets"
      "![?[3 4] *[1 2]]"
      (tagsquare "!" [
        tagsquare "?" [aint 3, aint 4],
        tagsquare "*" [aint 1, aint 2]])
