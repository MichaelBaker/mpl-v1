module Mpl.ASTToCoreSpec where

import Test.Hspec
import ASTHelpers (aparen, asquare, acurly, aint, afloat, atext, asym)

import Mpl.AST       (AST(..))
import Mpl.Core      (Core(..))
import Mpl.ASTToCore (astToCore)

test message ast core = it message (astToCore ast `shouldBe` core)

param name ty = aparen [asym ":", asym name, asym ty]

spec :: Spec
spec = do
  test "int"
    (AInt 1)
    (CInt 1)

  test "real"
    (AFloat 1.0)
    (CReal  1.0)

  test "text"
    (AText "a")
    (CText "a")
