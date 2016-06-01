module Mpl.TypeAnnotationSpec where

import Test.Hspec

import Mpl.AST            (Core(..), CoreType(..), emptyEnv)
import Mpl.TypeAnnotation (annotate)

import qualified Data.Map.Strict as Map

test message core annotatedCore = it message (annotate core `shouldBe` annotatedCore)

ty = Map.fromList

spec :: Spec
spec = do
  test "unit"  (CUnit  [0])           (CUnit  [0],           ty [([0], CUnitTy)])
  test "int"   (CInt   [0] 8)         (CInt   [0] 8,         ty [([0], CIntTy)])
  test "real"  (CReal  [0] 8.0)       (CReal  [0] 8.0,       ty [([0], CRealTy)])
  test "text"  (CText  [0] "a")       (CText  [0] "a",       ty [([0], CTextTy)])
  test "list"  (CList  [0] [])        (CList  [0] [],        ty [([0], CListTy)])
  test "map"   (CMap   [0] Map.empty) (CMap   [0] Map.empty, ty [([0], CMapTy)])
  test "assoc" (CAssoc [0] [])        (CAssoc [0] [],        ty [([0], CMapTy)])

  -- test "thunk"
  --   (CThunk () emptyEnv (CUnit ()))
  --   (CThunk (CThunkTy CUnitTy) emptyEnv (CUnit CUnitTy))

  -- test "function"
  --   (CFunc () emptyEnv ("a", CIntTy) (CUnit ()))
  --   (CFunc (CFuncTy CIntTy CUnitTy) emptyEnv ("a", CIntTy) (CUnit CUnitTy))

  -- test "forcing a thunk"
  --   (CForce () (CThunk () emptyEnv (CUnit ())))
  --   (CForce CUnitTy (CThunk (CThunkTy CUnitTy) emptyEnv (CUnit CUnitTy)))

  -- test "forcing an int"
  --   (CForce () (CInt () 1))
  --   (CForce CUnknownTy (CInt CIntTy 1))

  -- test "applying a function with matching arg"
  --   (CApp () (CFunc () emptyEnv ("a", CIntTy) (CUnit ())) (CInt () 1))
  --   (CApp CUnitTy (CFunc (CFuncTy CIntTy CUnitTy) emptyEnv ("a", CIntTy) (CUnit CUnitTy)) (CInt CIntTy 1))

  -- test "applying a function with non-mathching arg"
  --   (CApp () (CFunc () emptyEnv ("a", CIntTy) (CUnit ())) (CUnit ()))
  --   (CApp CUnknownTy (CFunc (CFuncTy CIntTy CUnitTy) emptyEnv ("a", CIntTy) (CUnit CUnitTy)) (CUnit CUnitTy))
