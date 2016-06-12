module Mpl.TypeInterpreterSpec where

import Test.Hspec

import Mpl.Core            (Core(..), Term)
import Mpl.TypeInterpreter (interpret)
import qualified Data.Map.Strict as Map

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
      (CTyLamApp
        (CTyLam "a" (CTyPrim "->" (CTyParam "a") (CTyParam "a")))
        CIntTy))
    (CLam "a" (CLamTy CIntTy CIntTy) (CSym "a"))

  describe "primitives" $ do
    describe "|" $ do
      test "creates a new record type with the union of the fields of the arguments"
        (CLam "a" (CTyPrim
          "|"
          (CRecordTy $ Map.fromList [("a", CIntTy)])
          (CRecordTy $ Map.fromList [("b", CIntTy), ("c", CIntTy)]))
          (CSym "a"))
        (CLam "a" (CRecordTy $ Map.fromList [("a", CIntTy), ("b", CIntTy), ("c", CIntTy)])
          (CSym "a"))
