module Mpl.CurrySpec where

import Test.Hspec
import Prelude hiding (curry)

import Mpl.AST   (AST(..), Type(..), meta)
import Mpl.Curry (curry)

curriesFuncWith message ast result = it ("a function with " ++ message) $ do
  curry ast `shouldBe` result

curriesAppWith message ast result = it ("an application with " ++ message) $ do
  curry ast `shouldBe` result

body                = AInt () 8
func paramList body = AFunc () paramList body
cfunc param body    = ACFunc () (Just param) body
app                 = AApp ()
capp f a            = ACApp () f (Just a)

spec :: Spec
spec = do
  curriesFuncWith "no parameters"                  (func [] body)                             (ACFunc () Nothing body)
  curriesFuncWith "one parameter"                  (func ["a"] body)                          (cfunc "a" body)
  curriesFuncWith "lots of parameters"             (func ["a", "b", "c", "d", "e"] body)      (cfunc "a" (cfunc "b" (cfunc "c" (cfunc "d" (cfunc "e" body)))))
  curriesFuncWith "an already curried function"    (cfunc "a" body)                           (cfunc "a" body)
  curriesAppWith  "no argument"                    (app (func [] body) [])                    (ACApp () (ACFunc () Nothing body) Nothing)
  curriesAppWith  "one argument"                   (app (func ["a"] body) [body])             (capp (cfunc "a" body) body)
  curriesAppWith  "many arguments"                 (app (func ["a"] body) [body, body, body]) (capp (capp (capp (cfunc "a" body) body) body) body)
  curriesAppWith  "an already curried application" (capp (cfunc "a" body) body)               (capp (cfunc "a" body) body)
