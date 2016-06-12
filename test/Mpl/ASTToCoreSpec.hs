module Mpl.ASTToCoreSpec where

import Test.Hspec
import ASTHelpers (aparen, asquare, acurly, aint, afloat, atext, asym, alam, tyan, poly, app, tylam, tagcurly)

import Mpl.AST       (AST(..))
import Mpl.Core      (Core(..))
import Mpl.ASTToCore (astToCore)
import qualified Data.Map.Strict as Map

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
      (CTyPrim "->" (CTyParam "a") (CTyParam "a"))
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
        (tylam "a" (aparen [asym "->", asym "a", asym "a"]))
        (asym "int")))
    (CPolyApp
      (CPolyFunc "t" (CLam "a" (CTyParam "t") (CSym "a")))
      (CTyLamApp
        (CTyLam "a" (CTyPrim "->" (CTyParam "a") (CTyParam "a")))
        CIntTy))

  test "record"
    (tagcurly "#" [asym "a", aint 1, asym "b", aint 2])
    (CRecord $ Map.fromList [("a", CInt 1), ("b", CInt 2)])

  test "record type"
    (tyan
      (tagcurly "#" [asym "a", asym "int", asym "b", asym "int"])
      (tagcurly "#" [asym "a", aint 1, asym "b", aint 2]))
    (CTyAnn
      (CRecordTy $ Map.fromList [("a", CIntTy), ("b", CIntTy)])
      (CRecord   $ Map.fromList [("a", CInt 1), ("b", CInt 2)]))

  test "type primtive"
    (aparen [
      asym "#",
      asquare [
        aparen [
          asym ":",
          asym "a",
          aparen [
            asym "|",
            tagcurly "#" [asym "a", asym "int"],
            tagcurly "#" [asym "b", asym "int", asym "c", asym "int"]]]],
      asym "a"])
    (CLam "a" (CTyPrim
      "|"
      (CRecordTy $ Map.fromList [("a", CIntTy)])
      (CRecordTy $ Map.fromList [("b", CIntTy), ("c", CIntTy)]))
      (CSym "a"))
