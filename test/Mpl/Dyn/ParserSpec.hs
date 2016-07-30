module Mpl.Dyn.ParserSpec where

import Test.Hspec
import Data.Text      (pack)
import Mpl.Dyn.AST    (AST(..))
import Mpl.Dyn.Parser (ParseType(..), toAST)
import qualified Text.Earley as E

test name filename parseType result = it name $ do
  source <- readFile ("test/TestCases/Dyn/Parser/" ++ filename)
  let (parses, report) = toAST parseType $ pack source
  if not (null $ E.expected report)
    then fail $ show report
    else parses `shouldBe` [result]

spec :: Spec
spec = do
  describe "integer" $ do
    test "a single digit" "integer-00.mpldyn" Exp (AInt 1)
    test "several digits" "integer-01.mpldyn" Exp (AInt 1234567890)

  describe "identifier" $ do
    test "constant" "constant-00.mpldyn" Prog (AProg [ADef (ASym "myConst") (AInt 123)])
