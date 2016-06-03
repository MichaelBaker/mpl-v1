module Mpl.TypeAnnotationSpec where

import Test.Hspec

import Mpl.AST            (Core(..), CoreType(..))
import Mpl.TypeAnnotation (annotate)

import qualified Data.Map.Strict as Map

test message core expectedTypes = it message $ do
  snd (annotate core) `shouldBe` Map.fromList expectedTypes

spec :: Spec
spec = do
  test "unit"  (CUnit  [0])           [([0], CUnitTy)]
  test "int"   (CInt   [0] 8)         [([0], CIntTy)]
  test "real"  (CReal  [0] 8.0)       [([0], CRealTy)]
  test "text"  (CText  [0] "a")       [([0], CTextTy)]
  test "list"  (CList  [0] [])        [([0], CListTy)]
  test "map"   (CMap   [0] Map.empty) [([0], CMapTy)]
  test "assoc" (CAssoc [0] [])        [([0], CMapTy)]

  test "thunk"
    (CThunk [0] () (CUnit [1]))
    [([0], CThunkTy CUnitTy), ([1], CUnitTy)]
