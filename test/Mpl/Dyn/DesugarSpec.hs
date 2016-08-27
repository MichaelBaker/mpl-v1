module Mpl.Dyn.DesugarSpec where

import Test.Hspec
import Mpl.Span             (emptySpan)
import Mpl.Dyn.Core         (Core(..), CoreLabel(..), CoreBind(..))
import Mpl.Dyn.Parser       (ParseType(Exp), parseFile, parseString)
import Mpl.Dyn.Desugar      (desugar)
import Text.Trifecta.Result (Result(Success, Failure))

testFile name filename expectedResult = it name $ do
  result <- parseFile Exp ("test/TestCases/Dyn/Desugar/" ++ filename)
  case result of
    Failure ex -> expectationFailure $ show ex
    Success a  -> desugar a `shouldBe` expectedResult

testString name string expectedResult = it name $ do
  let result = parseString Exp string
  case result of
    Failure ex -> expectationFailure $ show ex
    Success a  -> desugar a `shouldBe` expectedResult

sym     a   = CSym   a   emptySpan
int     a   = CInt   a   emptySpan
real    a   = CReal  a   emptySpan
utf16   a   = CUtf16 a   emptySpan
list    a   = CList  a   emptySpan
rec     a   = CRec   a   emptySpan
thunk   a   = CThunk a   emptySpan
let_exp a b = CLet   a b emptySpan
lam     a b = CLam   a b emptySpan
app     a b = CApp   a b emptySpan
lens    a   = CLens  a   emptySpan

symlbl a = CLSym a
intlbl a = CLInt a

bind a = CoreBind a

spec :: Spec
spec = do
  describe "symbol" $ do
    testString "simple symbol" "a" (sym "a")

  describe "integer" $ do
    testString "simple integer" "123" (int 123)

  describe "real" $ do
    testString "simple real" "123.0" (real 123.0)

  describe "utf16" $ do
    testString "simple utf16 string" "\"hello\"" (utf16 "hello")

  describe "list" $ do
    testString "list of ints" "[1, 2, 3]" (list [int 1, int 2, int 3])

  describe "record" $ do
    testString "simple record" "{a: 1, 2: 2}" (rec [(symlbl "a", int 1), (intlbl 2, int 2)])

  describe "let" $ do
    testString "simple let" "let a = 1 in a" (let_exp [(bind "a", int 1)] (sym "a"))

  describe "lambda" $ do
    testString "simple thunk" "(# 5)" (thunk (int 5))
    testString "simple lambda" "(# a = a)" (lam (bind "a") (sym "a"))
    testString "multiple params" "(# a b = a)" (lam (bind "a") (lam (bind "b") (sym "a")))

  describe "application" $ do
    testString "simple application" "a b" (app (sym "a") (sym "b"))
    testString "multiple arguments" "(a b c)" (app (app (sym "a") (sym "b")) (sym "c"))
    testString "int application"    "(3 4)" (app (int 3) (int 4))
    testString "lens application"   "a.b.c.3" (app (lens [sym "b", sym "c", int 3]) (sym "a"))