module Mpl.TypeAnnotationSpec where

import Test.Hspec

import Mpl.AST            (Core(..), CoreType(..))
import Mpl.TypeAnnotation (annotate)

test message core annotatedCore = it message (annotate core `shouldBe` annotatedCore)

spec :: Spec
spec = do
  test "unit"  (CUnit  ())            (CUnit  CUnitTy)
  test "int"   (CInt   () 8)          (CInt   CIntTy 8)
  test "real"  (CReal  () 8.0)        (CReal  CRealTy 8.0)
  test "text"  (CText  () "a")        (CText  CTextTy "a")
  test "thunk" (CThunk () (CUnit ())) (CThunk (CThunkTy CUnitTy) (CUnit CUnitTy))
