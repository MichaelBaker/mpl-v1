module Mpl.TypeInterpreterSpec where

import Test.Hspec
import qualified Data.Map as Map

import Mpl.AST             (AST(..), Core(..), CoreType(..))
import Mpl.TypeInterpreter (interpretTypes)

test :: String -> Core () () -> Core () () -> Spec
test message core expectedCore = it message $ do
  interpretTypes core `shouldBe` expectedCore

spec :: Spec
spec = do
  test "applies type functions"
    (CApp () (CTyFunc () () ("t") (CFunc () () ("a", "t") (CUnit ()))) (CIdent () "unit"))
    (CFunc () () ("a", "unit") (CUnit ()))

  test "marks unbound types as unknown"
    (CFunc () () ("a", "b") (CUnit ()))
    (CFunc () () ("a", "unknown") (CUnit ()))

  test "marks incorrectly named types as unknown"
    (CApp () (CTyFunc () () ("t") (CFunc () () ("a", "z") (CUnit ()))) (CIdent () "unit"))
    (CFunc () () ("a", "unknown") (CUnit ()))

  test "leaves unit unaltered"
    (CUnit ())
    (CUnit ())

  test "leaves int unaltered"
    (CInt () 1)
    (CInt () 1)

  test "leaves real unaltered"
    (CReal () 1.0)
    (CReal () 1.0)

  test "leaves text unaltered"
    (CText () "a")
    (CText () "a")

  test "leaves ident unaltered"
    (CIdent () "a")
    (CIdent () "a")

  test "leaves list unaltered"
    (CList () [])
    (CList () [])

  test "leaves assoc unaltered"
    (CAssoc () [])
    (CAssoc () [])

  test "leaves map unaltered"
    (CMap () Map.empty)
    (CMap () Map.empty)

  test "leaves thunk unaltered"
    (CThunk () () (CUnit ()))
    (CThunk () () (CUnit ()))
