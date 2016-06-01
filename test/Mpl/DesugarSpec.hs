module Mpl.DesugarSpec where

import Test.Hspec

import Mpl.AST     (AST(..), ASTType(..), Core(..), CoreType(..), emptyEnv)
import Mpl.Desugar (desugar)

test message ast core = it message (desugar ast `shouldBe` core)

spec :: Spec
spec = do
  test "unit  -> unit"  (AUnit  ())     (CUnit  ())
  test "int   -> int"   (AInt   () 8)   (CInt   () 8)
  test "float -> real"  (AFloat () 8.0) (CReal  () 8.0)
  test "text  -> text"  (AText  () "a") (CText  () "a")
  test "ident -> ident" (AIdent () "a") (CIdent () "a")
  test "list  -> list"  (AList  () [])  (CList  () [])
  test "map   -> assoc" (AMap   () [])  (CAssoc () [])

  test "a function with no arguments to a tunk"
    (AFunc  () [] (AUnit ()))
    (CThunk () emptyEnv (CUnit ()))

  test "an application with no arguments as a force"
    (AApp   () (AUnit ()) [])
    (CForce () (CUnit ()))

  test "a function of one argument"
    (AFunc () [("a", AIntTy)] (AUnit ()))
    (CFunc () emptyEnv ("a", CIntTy) (CUnit ()))

  test "curries a function of multiple arguments"
    (AFunc () [("a", AIntTy), ("b", AIntTy)] (AUnit ()))
    (CFunc () emptyEnv ("a", CIntTy) (CFunc () emptyEnv ("b", CIntTy) (CUnit ())))

  test "application of a single argument"
    (AApp () (AUnit ()) [AUnit ()])
    (CApp () (CUnit ()) (CUnit ()))

  test "curries application of multiple arguments"
    (AApp () (AUnit ()) [AInt () 1, AInt () 2])
    (CApp () (CApp () (CUnit ()) (CInt () 1)) (CInt () 2))
