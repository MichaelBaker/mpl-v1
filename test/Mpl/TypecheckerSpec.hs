module Mpl.TypecheckerSpec where

import Test.Hspec

import Mpl.AST           (AST(..), Type(..), meta)
import Mpl.Typechecker   (typecheck)
import Control.Exception (evaluate)

hasType message ast t = it message $ do
  meta (typecheck ast) `shouldBe` t

aint      = AInt   ()
atext     = AText  ()
aident    = AIdent ()
afunc     = AFunc  ()
acfunc    = ACFunc () . Just
afloat    = AFloat ()
amap      = AMap   ()
alist     = AList  ()
acapp f a = ACApp  () f (Just a)

spec :: Spec
spec = do
  describe "typechecking" $ do
    hasType "an integer" (aint 8)      IntType
    hasType "a float"    (afloat 8.0)  FloatType
    hasType "text"       (atext "a")   TextType
    hasType "empty list" (alist [])    (ListType Unknown)
    hasType "empty map"  (amap [])     (MapType  Unknown Unknown)

    hasType "homogenous list"
      (alist [aint 8, aint 9])
      (ListType IntType)

    hasType "heterogeneous list"
      (alist [aint 8, atext "a"])
      (ListType Unknown)

    hasType "homogenous map"
      (amap [(atext "a", aint 8), (atext "b", aint 9)])
      (MapType TextType IntType)

    hasType "heterogeneous map values"
      (amap [(atext "a", aint 8), (atext "c", atext "b")])
      (MapType TextType Unknown)

    hasType "heterogeneous map keys"
      (amap [(atext "a", aint 8), (aint 2, aint 9)])
      (MapType Unknown IntType)

    hasType "heterogeneous map keys and values"
      (amap [(atext "a", aint 8), (aint 2, atext "a")])
      (MapType Unknown Unknown)

    hasType "identity function"
      (acfunc "a" (aident "a"))
      (FuncType Unknown Unknown)

    hasType "const function"
      (acfunc "a" (aint 8))
      (FuncType Unknown IntType)

    hasType "applied identity function"
      (acapp (acfunc "a" (aident "a")) (aint 8))
      IntType

    it "assumes there are no un-curried functions" $ do
      evaluate (typecheck (afunc ["a"] (aint 8))) `shouldThrow` anyException
