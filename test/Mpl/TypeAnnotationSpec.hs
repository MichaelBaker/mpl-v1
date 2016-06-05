module Mpl.TypeAnnotationSpec where

import Test.Hspec

import Mpl.Core           (Core(..), CoreType(..))
import Mpl.TypeAnnotation (annotate)

import qualified Data.Map.Strict as Map

test :: String -> Core Int () -> [(Int, CoreType)] -> Spec
test message core expectedTypes = it message $ do
  snd (annotate core) `shouldBe` Map.fromList expectedTypes

spec :: Spec
spec = do
  test "unit"  (CUnit  0)           [(0, CUnitTy)]
  test "int"   (CInt   0 8)         [(0, CIntTy)]
  test "real"  (CReal  0 8.0)       [(0, CRealTy)]
  test "text"  (CText  0 "a")       [(0, CTextTy)]
  test "list"  (CList  0 [])        [(0, CListTy)]
  test "map"   (CMap   0 Map.empty) [(0, CMapTy)]
  test "assoc" (CAssoc 0 [])        [(0, CMapTy)]

  test "thunk"
    (CThunk 0 () (CUnit 1))
    [(0, CThunkTy CUnitTy), (1, CUnitTy)]

  test "function"
    (CFunc 0 () ("a", CUnitTy) (CUnit 1))
    [(0, CFuncTy CUnitTy CUnitTy), (1, CUnitTy)]

  test "forcing a thunk"
    (CForce 0 (CThunk 1 () (CInt 2 1)))
    [(0, CIntTy), (1, CThunkTy CIntTy), (2, CIntTy)]

  test "forcing not a thunk"
    (CForce 0 (CReal 1 1.0))
    [(0, CUnknownTy), (1, CRealTy)]

  test "applying a well typed function"
    (CApp 0 (CFunc 1 () ("a", CUnitTy) (CInt 2 1)) (CUnit 3))
    [(0, CIntTy), (1, CFuncTy CUnitTy CIntTy), (2, CIntTy), (3, CUnitTy)]

  test "applying an ill-typed function"
    (CApp 0 (CFunc 1 () ("a", CRealTy) (CInt 2 1)) (CUnit 3))
    [(0, CUnknownTy), (1, CFuncTy CRealTy CIntTy), (2, CIntTy), (3, CUnitTy)]

  test "applying not a function"
    (CApp 0 (CInt 1 2) (CUnit 2))
    [(0, CUnknownTy), (1, CIntTy), (2, CUnitTy)]
