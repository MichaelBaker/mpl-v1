module Mpl.CurrySpec where

import Test.Hspec
import Prelude hiding (curry)

import Mpl.AST   (AST(..), Type(..), meta)
import Mpl.Curry (curry)

curries message ast result = it ("a function with " ++ message) $ do
  curry ast `shouldBe` result

func paramList body = AFunc () (map (AIdent ()) paramList) body
body = AInt () 8

spec :: Spec
spec = do
  curries "no parameters"      (func [] body)                        (func [] body)
  curries "one parameter"      (func ["a"] body)                     (func ["a"] body)
  curries "two parameters"     (func ["a", "b"] body)                (func ["a"] (func ["b"] body))
  curries "lots of parameters" (func ["a", "b", "c", "d", "e"] body) (func ["a"] (func ["b"] (func ["c"] (func ["d"] (func ["e"] body)))))
