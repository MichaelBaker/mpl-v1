module Mpl.Dyn.ParserSpec where

import Test.Hspec
import Helper.AST
import Helper.Test
import Mpl.Dyn.Parser (ParseType(..), parseFile, parseString)

testFile   = makeTestFile "test/TestCases/Dyn/Parser/" parseFile id
testString = makeTestString parseString id

spec :: Spec
spec = do
  describe "integer" $ do
    testFile "a single digit" "integer-00.mpldyn" Exp (dyn_int 1)
    testFile "several digits" "integer-01.mpldyn" Exp (dyn_int 1234567890)
    testFile "negative"       "integer-02.mpldyn" Exp (dyn_int (-1234567890))

  describe "real" $ do
    testFile "a single digit" "real-00.mpldyn" Exp (dyn_real 1.0)
    testFile "several digits" "real-01.mpldyn" Exp (dyn_real 1234567890.0)
    testFile "negative"       "real-02.mpldyn" Exp (dyn_real (-1234567890.0))

  describe "utf16" $ do
    testString "a simple string" "\"hello\"" Exp (dyn_text "hello")

  describe "symbol" $ do
    testString "an alphabetic symbol" "abc" Exp (dyn_sym "abc")

  describe "definition" $ do
    testFile "constant"          "definition-00.mpldyn" Def (dyn_def (dyn_sym "myConst") (dyn_int 123))
    testFile "function constant" "definition-01.mpldyn" Def (dyn_def (dyn_sym "fun") (dyn_lam [] (dyn_real 2.0)))
    testFile "function sugar"    "definition-02.mpldyn" Def (dyn_def (dyn_sym "fun") (dyn_lam [dyn_sym "a", dyn_sym "b"] (dyn_real 3.0)))

  describe "lambda" $ do
    testFile "lambda with zero arguments"  "lambda-00.mpldyn" Exp (dyn_lam [] (dyn_int 9))
    testFile "lambda with one argument"    "lambda-01.mpldyn" Exp (dyn_lam [dyn_sym "a"] (dyn_int 9))
    testFile "lambda with three arguments" "lambda-02.mpldyn" Exp (dyn_lam [dyn_sym "a", dyn_sym "b", dyn_sym "c"] (dyn_int 9))

  describe "record" $ do
    testString "empty record"                   "{}"     Exp (dyn_rec [])
    testString "record with one symbolic field" "{a: 1}" Exp (dyn_rec [dyn_field (dyn_sym "a") (dyn_int 1)])
    testString "record with one numberic field" "{0: 1}" Exp (dyn_rec [dyn_field (dyn_int 0) (dyn_int 1)])
    testFile   "record with multiple fields and trailing comma" "record-00.mpldyn" Exp (dyn_rec [
      dyn_field (dyn_int 0) (dyn_int 1),
      dyn_field (dyn_sym "a") (dyn_int 3),
      dyn_field (dyn_sym "b") (dyn_sym "c")
      ])

  describe "list" $ do
    testString "empty list" "[]" Exp (dyn_list [])
    testFile "list with multiple elements and trailing comma" "list-00.mpldyn" Exp (dyn_list [
      (dyn_int 0),
      (dyn_rec [dyn_field (dyn_sym "a") (dyn_int 3)]),
      (dyn_real 32.1),
      (dyn_sym "a")
      ])

  describe "lens" $ do
    testString "field label"      ".hello"      Exp (dyn_lens [dyn_sym "hello"])
    testString "int label"        ".0"          Exp (dyn_lens [dyn_int 0])
    testString "expression label" ".(1.234)"    Exp (dyn_lens [dyn_real 1.234])
    testString "multiple lenses"  ".a.0.(1.23)" Exp (dyn_lens [dyn_sym "a", dyn_int 0, dyn_real 1.23])

  describe "application" $ do
    testString "simple application" "a b" Exp (dyn_app (dyn_sym "a") [dyn_sym "b"])
    testString "paren application"  "(a b)" Exp (dyn_app (dyn_sym "a") [dyn_sym "b"])
    testString "int application"    "(3 4)" Exp (dyn_app (dyn_int 3) [dyn_int 4])
    testString "lens application"   "a.b.c.3" Exp (dyn_lensapp (dyn_lens [dyn_sym "b", dyn_sym "c", dyn_int 3]) (dyn_sym "a"))
    testFile   "nested application" "application-00.mpldyn" Exp (dyn_app (dyn_sym "myFun") [
      dyn_int 1,
      dyn_int 2,
      dyn_int 3
      ])
    testFile   "nested application" "application-01.mpldyn" Exp (dyn_app (dyn_sym "myFun") [
      dyn_int 1,
      dyn_app (dyn_sym "f") [
        dyn_int 4,
        dyn_int 2
      ],
      dyn_real 4.2,
      dyn_app (dyn_sym "g") [
        dyn_app (dyn_int 3) [
          dyn_int 4
        ]
      ]
      ])

  describe "let" $ do
    testString "simple let" "let a = 5 in a" Exp (dyn_let_exp [dyn_def (dyn_sym "a") (dyn_int 5)] $ dyn_sym "a")

  describe "program" $ do
    testFile "two constants" "program-00.mpldyn" Prog (dyn_prog $ dyn_recdefs [
      dyn_def (dyn_sym "myConst")    (dyn_int 123),
      dyn_def (dyn_sym "otherConst") (dyn_int 890)
      ])

    testFile "function sugar" "program-01.mpldyn" Prog (dyn_prog $ dyn_recdefs [
      dyn_def (dyn_sym "f") (dyn_lam [dyn_sym "a", dyn_sym "b"] (dyn_int 123))
      ])

    testFile "two function sugar" "program-02.mpldyn" Prog (dyn_prog $ dyn_recdefs [
      dyn_def (dyn_sym "f") (dyn_lam [dyn_sym "a", dyn_sym "b"] (dyn_int 123)),
      dyn_def (dyn_sym "g") (dyn_lam [dyn_sym "a", dyn_sym "b"] (dyn_sym "a"))
      ])
