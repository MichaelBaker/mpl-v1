module Mpl.ASTToCoreSpec where

import Test.Hspec
import ASTHelpers (aparen, asquare, aint, asym)

import Mpl.AST       (AST(..))
import Mpl.Core      (Core(..), Type(..))
import Mpl.ASTToCore (toCore)

test message ast core = it message $ do
  toCore ast `shouldBe` (Right core)

spec :: Spec
spec = do
  test "int -> int"   (aint 8)   (CInt [0] TInt 8)
  test "sym -> ident" (asym "a") (CIdent [0] TUnknown "a")

  test "a function with no arguments to a thunk"
    (aparen [asym "#", asquare [], aint 8])
    (CThunk [0] TUnknown (CInt [0, 0] TInt 8))

  test "a function of one argument"
    (aparen [asym "#", asquare [asym "a"], aint 1])
    (CFunc [0] TUnknown "a" (CInt [0, 0] TInt 1))

  test "curries a function of multiple arguments"
    (aparen [asym "#", asquare [asym "a", asym "b"], asym "a"])
    (CFunc [0] TUnknown "a" (CFunc [0, 0] TUnknown "b" (CIdent [0, 0, 0] TUnknown "a")))

  test "an application with no arguments as a force"
    (aparen [aint 4])
    (CForce [0] TUnknown (CInt [0, 0] TInt 4))

  test "application of a single argument"
    (aparen [aint 2, aint 9])
    (CApp [0] TUnknown (CInt [0, 0] TInt 2) (CInt [1, 0] TInt 9))

  test "curries application of multiple arguments"
    (aparen [aint 0, aint 1, aint 2])
    (CApp [0] TUnknown
      (CApp [0, 0] TUnknown
        (CInt [0, 0, 0] TInt 0)
        (CInt [1, 0, 0] TInt 1))
      (CInt [1, 0] TInt 2))
