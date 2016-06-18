module Mpl.ASTToCoreSpec where

import Test.Hspec
import ASTHelpers (aparen, asquare, aint, asym)

import Mpl.AST       (AST(..), Core(..), emptyEnv)
import Mpl.ASTToCore (astToCore)

test message ast core = it message (astToCore ast `shouldBe` core)

spec :: Spec
spec = do
  test "int   -> int"   (aint 8)   (CInt [0] 8)
  test "ident -> ident" (asym "a") (CIdent [0] "a")

  test "a function with no arguments to a thunk"
    (aparen [asym "#", asquare [], aint 8])
    (CThunk [0] emptyEnv (CInt [0, 0] 8))

  test "a function of one argument"
    (aparen [asym "#", asquare [asym "a"], aint 1])
    (CFunc [0] emptyEnv "a" (CInt [0, 0] 1))

  test "curries a function of multiple arguments"
    (aparen [asym "#", asquare [asym "a", asym "b"], asym "a"])
    (CFunc [0] emptyEnv "a" (CFunc [0, 0] emptyEnv "b" (CIdent [0, 0, 0] "a")))

  test "an application with no arguments as a force"
    (aparen [aint 4])
    (CForce [0] (CInt [0, 0] 4))

  test "application of a single argument"
    (aparen [aint 2, aint 9])
    (CApp [0] (CInt [0, 0] 2) (CInt [1, 0] 9))

  test "curries application of multiple arguments"
    (aparen [aint 0, aint 1, aint 2])
    (CApp [0] (CApp [0, 0] (CInt [0, 0, 0] 0) (CInt [1, 0, 0] 1)) (CInt [1, 0] 2))
