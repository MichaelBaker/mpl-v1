module Mpl.TypeCheckerSpec where

import Test.Hspec

import Mpl.Core        (Core(..))
import Mpl.TypeChecker (TypeError(..), typeContradictions)

test name core result = it name $ do
  typeContradictions core `shouldBe` result

spec :: Spec
spec = do
  test "a simple type error"
    (CTermApp
      (CLam "a" CIntTy (CSym "a"))
      (CText "a"))
    [Contradiction CIntTy CTextTy (CTermApp
      (CLam "a" CIntTy (CSym "a"))
      (CText "a"))]
