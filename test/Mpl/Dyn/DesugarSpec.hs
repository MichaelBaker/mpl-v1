module Mpl.Dyn.DesugarSpec where

import Test.Hspec
import Helper.Test
import Mpl.Span             (emptySpan)
import Mpl.Dyn.Core         (Core(..), CoreLabel(..), CoreBind(..))
import Mpl.Dyn.Parser       (ParseType(Exp), parseFile, parseString)
import Mpl.Dyn.Desugar      (desugar)

testFile   = makeTestFile "test/TestCases/Dyn/Desugar/" parseFile desugar
testString = makeTestString parseString desugar

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
    testString "simple symbol" "a" Exp (sym "a")

  describe "integer" $ do
    testString "simple integer" "123" Exp (int 123)

  describe "real" $ do
    testString "simple real" "123.0" Exp (real 123.0)

  describe "utf16" $ do
    testString "simple utf16 string" "\"hello\"" Exp (utf16 "hello")

  describe "list" $ do
    testString "list of ints" "[1, 2, 3]" Exp (list [int 1, int 2, int 3])

  describe "record" $ do
    testString "simple record" "{a: 1, 2: 2}" Exp (rec [(symlbl "a", int 1), (intlbl 2, int 2)])

  describe "let" $ do
    testString "simple let" "let a = 1 in a" Exp (let_exp [(bind "a", int 1)] (sym "a"))

  describe "lambda" $ do
    testString "simple thunk" "(# 5)" Exp (thunk (int 5))
    testString "simple lambda" "(# a = a)" Exp (lam (bind "a") (sym "a"))
    testString "multiple params" "(# a b = a)" Exp (lam (bind "a") (lam (bind "b") (sym "a")))

  describe "application" $ do
    testString "simple application" "a b" Exp (app (sym "a") (sym "b"))
    testString "multiple arguments" "(a b c)" Exp (app (app (sym "a") (sym "b")) (sym "c"))
    testString "int application"    "(3 4)" Exp (app (int 3) (int 4))
    testString "lens application"   "a.b.c.3" Exp (app (lens [sym "b", sym "c", int 3]) (sym "a"))
