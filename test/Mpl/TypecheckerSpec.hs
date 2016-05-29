module Mpl.TypecheckerSpec where

import Test.Hspec

import Mpl.Parser      (AST(..))
import Mpl.Typechecker (TAST(..), Type(..), typecheck)

hasType message ast t = it message $ do
  fst (typecheck ast) `shouldBe` t

spec :: Spec
spec = do
  describe "typechecking" $ do
    hasType "an integer" (AInt "8")      IntType
    hasType "a float"    (AFloat "8.0")  FloatType
    hasType "text"       (AText "a")     TextType
    hasType "empty list" (AList [])      (ListType Unknown)
    hasType "empty map"  (AMap [])       (MapType  Unknown Unknown)

    hasType "homogenous list"
      (AList [AInt "8", AInt "9"])
      (ListType IntType)

    hasType "heterogeneous list"
      (AList [AInt "8", AText "a"])
      (ListType Unknown)

    hasType "homogenous map"
      (AMap [(AText "a", AInt "8"), (AText "b", AInt "9")])
      (MapType TextType IntType)

    hasType "heterogeneous map values"
      (AMap [(AText "a", AInt "8"), (AText "c", AText "b")])
      (MapType TextType Unknown)

    hasType "heterogeneous map keys"
      (AMap [(AText "a", AInt "8"), (AInt "2", AInt "9")])
      (MapType Unknown IntType)

    hasType "heterogeneous map keys and values"
      (AMap [(AText "a", AInt "8"), (AInt "2", AText "a")])
      (MapType Unknown Unknown)

    hasType "identity function"
      (AFunc (AList [AIdent "a"]) (AIdent "a"))
      (FuncType [Unknown] Unknown)
