module Mpl.TypeAnnotationSpec where

import Test.Hspec

import Mpl.AST            (Core(..), CoreType(..), emptyEnv)
import Mpl.TypeAnnotation (annotate)

import qualified Data.Map.Strict as Map

test message core annotatedCore = it message (annotate core `shouldBe` annotatedCore)

spec :: Spec
spec = do
  test "unit"  (CUnit  ())           (CUnit  CUnitTy)
  test "int"   (CInt   () 8)         (CInt   CIntTy 8)
  test "real"  (CReal  () 8.0)       (CReal  CRealTy 8.0)
  test "text"  (CText  () "a")       (CText  CTextTy "a")
  test "list"  (CList  () [])        (CList  CListTy [])
  test "map"   (CMap   () Map.empty) (CMap   CMapTy Map.empty)
  test "assoc" (CAssoc () [])        (CAssoc CMapTy [])

  test "thunk"
    (CThunk () emptyEnv (CUnit ()))
    (CThunk (CThunkTy CUnitTy) emptyEnv (CUnit CUnitTy))

  test "function"
    (CFunc () emptyEnv ("a", CIntTy) (CUnit ()))
    (CFunc (CFuncTy CIntTy CUnitTy) emptyEnv ("a", CIntTy) (CUnit CUnitTy))

  test "forcing a thunk"
    (CForce () (CThunk () emptyEnv (CUnit ())))
    (CForce CUnitTy (CThunk (CThunkTy CUnitTy) emptyEnv (CUnit CUnitTy)))

  test "forcing an int"
    (CForce () (CInt () 1))
    (CForce CUnknownTy (CInt CIntTy 1))

  test "applying a function with matching arg"
    (CApp () (CFunc () emptyEnv ("a", CIntTy) (CUnit ())) (CInt () 1))
    (CApp CUnitTy (CFunc (CFuncTy CIntTy CUnitTy) emptyEnv ("a", CIntTy) (CUnit CUnitTy)) (CInt CIntTy 1))

  test "applying a function with non-mathching arg"
    (CApp () (CFunc () emptyEnv ("a", CIntTy) (CUnit ())) (CUnit ()))
    (CApp CUnknownTy (CFunc (CFuncTy CIntTy CUnitTy) emptyEnv ("a", CIntTy) (CUnit CUnitTy)) (CUnit CUnitTy))
