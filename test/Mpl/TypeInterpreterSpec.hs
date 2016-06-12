module Mpl.TypeInterpreterSpec where

import Test.Hspec

import Mpl.Core            (Core(..), Term)
import Mpl.TypeInterpreter (interpret)

test :: String -> Core Term -> Core Term -> Spec
test message core expectedCore = it message $ do
  interpret core `shouldBe` expectedCore

spec :: Spec
spec = do
  test "applies type functions"
    (CPolyApp
      (CPolyFunc "t" (CLam "a" (CTyParam "t") (CSym "a")))
      CIntTy)
    (CLam "a" CIntTy (CSym "a"))

  test "removes type annotations"
    (CTyAnn CIntTy (CInt 5))
    (CInt 5)

  test "applies type operators"
    (CPolyApp
      (CPolyFunc "t" (CLam "a" (CTyParam "t") (CSym "a")))
      (CTyOpApp
        (CTyOp "a" (CLamTy (CTyParam "a") (CTyParam "a")))
        CIntTy))
    (CLam "a" (CLamTy CIntTy CIntTy) (CSym "a"))
