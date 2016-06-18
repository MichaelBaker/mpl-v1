module Mpl.CompilerSpec where

import Test.Hspec

import Mpl.Compiler (Error(..), compile)

spec :: Spec
spec = do
  describe "1.0" $ do
    test_1_0 "a single int"                "8907" (Success "8907")
    test_1_0 "forcing a thunk"             "((# [] 5))" (Success "5")
    test_1_0 "simple function application" "((# [a] a) 5)" (Success "5")
    test_1_0 "simple closure"              "((# [a] ((# [_] a) 1)) 5)" (Success "5")
    test_1_0 "deep closure"                "((# [a] ((# [_] ((# [_] ((# [_] ((# [_] a) 1)) 2)) 3)) 4)) 5)" (Success "5")
    test_1_0 "multiple parameters"         "((# [a b] a) 1 2)" (Success "1")
    test_1_0 "partial application"         "((# [a b] a) 1)" (Success "(# [b] a)")
    test_1_0 "runtime type error"          "(1 1)" RuntimeError

test_1_0 name string testType = it name $ do
  let result = compile string
  case testType of
    Success expected -> resultType result `shouldBe` (SuccessResult expected)
    RuntimeError     -> resultType result `shouldBe` RuntimeErrorResult

data TestType
  = Success String
  | RuntimeError

data ResultType
  = SuccessResult String
  | ParseErrorResult
  | ASTToCoreErrorResult
  | RuntimeErrorResult
  deriving (Show, Eq)

resultType (Right s)     = SuccessResult s
resultType (Left (PE _)) = ParseErrorResult
resultType (Left (AC _)) = ASTToCoreErrorResult
resultType (Left (RE _)) = RuntimeErrorResult
