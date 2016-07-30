module Mpl.Dyn.ParserSpec where

import Test.Hspec
import Mpl.Dyn.AST    (AST(..))
import Mpl.Dyn.Parser (toAST)

test name filename result = it name $ do
  source <- readFile ("test/TestCases/Dyn/Parser/" ++ filename)
  let (parses, _) = toAST source
  parses `shouldBe` [result]

spec :: Spec
spec = do
  describe "integer" $ do
    test "a single digit" "integer-00.mpldyn" (AInt 1)
