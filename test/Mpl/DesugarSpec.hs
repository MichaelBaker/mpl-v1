module Mpl.DesugarSpec where

import Test.Hspec

import Mpl.AST     (AST(..), Core(..), CoreType(..), emptyEnv)
import Mpl.Desugar (desugar)

test message ast core = it message (desugar ast `shouldBe` core)

-- TODO: Update these
spec :: Spec
spec = do
  return ()
  -- test "unit  -> unit"  (AUnit  ())     (CUnit  [0])
  -- test "int   -> int"   (AInt   () 8)   (CInt   [0] 8)
  -- test "float -> real"  (AFloat () 8.0) (CReal  [0] 8.0)
  -- test "text  -> text"  (AText  () "a") (CText  [0] "a")
  -- test "ident -> ident" (AIdent () "a") (CIdent [0] "a")
  -- test "list  -> list"  (AList  () [])  (CList  [0] [])
  -- test "map   -> assoc" (AMap   () [])  (CAssoc [0] [])

  -- test "a function with no arguments to a thunk"
  --   (AFunc  () [] (AUnit ()))
  --   (CThunk [0] emptyEnv (CUnit [0, 0]))

  -- test "an application with no arguments as a force"
  --   (AApp   () (AUnit ()) [])
  --   (CForce [0] (CUnit [0, 0]))

  -- test "a function of one argument"
  --   (AFunc () [("a", AIntTy)] (AUnit ()))
  --   (CFunc [0] emptyEnv ("a", CIntTy) (CUnit [0, 0]))

  -- test "curries a function of multiple arguments"
  --   (AFunc () [("a", AIntTy), ("b", AIntTy)] (AUnit ()))
  --   (CFunc [0] emptyEnv ("a", CIntTy) (CFunc [0, 0] emptyEnv ("b", CIntTy) (CUnit [0, 0, 0])))

  -- test "application of a single argument"
  --   (AApp () (AUnit ()) [AUnit ()])
  --   (CApp [0] (CUnit [0, 0]) (CUnit [1, 0]))

  -- test "curries application of multiple arguments"
  --   (AApp () (AUnit ()) [AInt () 1, AInt () 2])
  --   (CApp [0] (CApp [0, 0] (CUnit [0, 0, 0]) (CInt [1, 0, 0] 1)) (CInt [1, 0] 2))
