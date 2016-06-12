module Mpl.ASTToCoreSpec where

import Test.Hspec
import ASTHelpers (aparen, asquare, acurly, aint, afloat, atext, asym, alam, tyan, poly, app, tyop)

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

  test "a lambda"
    (alam "a" "int" $ asym "a")
    (CLam "a" CIntTy (CSym "a"))

  test "applying a lambda to a term"
    (aparen [
      (alam "a" "t" $ asym "a"),
      aint 1])
    (CTermApp (CLam "a" (CTyParam "t") (CSym "a")) (CInt 1))

  test "annotating a lambda with a type"
    (tyan
      (aparen [asym "->", asym "a", asym "a"])
      (alam "a" "t" $ asym "a"))
    (CTyAnn
      (CLamTy (CTyParam "a") (CTyParam "a"))
      (CLam "a" (CTyParam "t") (CSym "a")))

  test "annotating an integer with a type"
    (tyan
      (asym "int")
      (aint 5))
    (CTyAnn
      CIntTy
      (CInt 5))

  test "applied polymorphic fuction"
    (app (poly "t" (alam "a" "t" $ asym "a")) (asym "int"))
    (CPolyApp
      (CPolyFunc "t" (CLam "a" (CTyParam "t") (CSym "a")))
      CIntTy)

  test "applied type operator"
    (app
      (poly "t" (alam "a" "t" $ asym "a"))
      (app
        (tyop "a" (aparen [asym "->", asym "a", asym "a"]))
        (asym "int")))
    (CPolyApp
      (CPolyFunc "t" (CLam "a" (CTyParam "t") (CSym "a")))
      (CTyOpApp
        (CTyOp "a" (CLamTy (CTyParam "a") (CTyParam "a")))
        CIntTy))
