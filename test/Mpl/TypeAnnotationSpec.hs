module Mpl.TypeAnnotationSpec where

import Test.Hspec

import Mpl.AST            (Core(..), CoreType(..))
import Mpl.TypeAnnotation (annotate)

test message core annotatedCore = it message (annotate core `shouldBe` annotatedCore)

spec :: Spec
spec = do
  test "unit"  (CUnit  ())     (CUnit  CUnitTy)
  test "int"   (CInt   () 8)   (CInt   CIntTy 8)
  test "real"  (CReal  () 8.0) (CReal  CRealTy 8.0)
  test "text"  (CText  () "a") (CText  CTextTy "a")
  test "list"  (CList  () [])  (CList  CListTy [])
  test "map"   (CMap   () [])  (CMap   CMapTy [])

  test "thunk"
    (CThunk () (CUnit ()))
    (CThunk (CThunkTy CUnitTy) (CUnit CUnitTy))

  test "function"
    (CFunc () ("a", CIntTy) (CUnit ()))
    (CFunc (CFuncTy CIntTy CUnitTy) ("a", CIntTy) (CUnit CUnitTy))

  test "forcing a thunk"
    (CForce () (CThunk () (CUnit ())))
    (CForce CUnitTy (CThunk (CThunkTy CUnitTy) (CUnit CUnitTy)))

  test "forcing an int"
    (CForce () (CInt () 1))
    (CForce CUnknownTy (CInt CIntTy 1))

  test "applying a function with matching arg"
    (CApp () (CFunc () ("a", CIntTy) (CUnit ())) (CInt () 1))
    (CApp CUnitTy (CFunc (CFuncTy CIntTy CUnitTy) ("a", CIntTy) (CUnit CUnitTy)) (CInt CIntTy 1))

  test "applying a function with non-mathching arg"
    (CApp () (CFunc () ("a", CIntTy) (CUnit ())) (CUnit ()))
    (CApp CUnknownTy (CFunc (CFuncTy CIntTy CUnitTy) ("a", CIntTy) (CUnit CUnitTy)) (CUnit CUnitTy))
