module Mpl.TypeInterpreterSpec where

import Test.Hspec
import qualified Data.Map as Map

import Mpl.Core            (Core(..), CoreType(..))
import Mpl.TypeInterpreter (interpretTypes)

test :: String -> Core () () -> Core () () -> Spec
test message core expectedCore = it message $ do
  interpretTypes core `shouldBe` expectedCore

spec :: Spec
spec = do
  test "applies type functions"
    (CTyApp () (CTyFunc () () "t" (CFunc () () ("a", CTSym "t") (CUnit ()))) CUnitTy)
    (CFunc () () ("a", CUnitTy) (CUnit ()))

  test "marks unbound types as unknown"
    (CFunc () () ("a", CTSym "b") (CUnit ()))
    (CFunc () () ("a", CUnknownTy) (CUnit ()))

  test "marks incorrectly named types as unknown"
    (CTyApp () (CTyFunc () () "t" (CFunc () () ("a", CTSym "z") (CUnit ()))) CUnitTy)
    (CFunc () () ("a", CUnknownTy) (CUnit ()))

  test "applies type operators"
    (CTyApp ()
      (CTyFunc () () "t" (CFunc () () ("a", CTSym "t") (CIdent () "a")))
      (CTApp (CTFOmega "t" (CTSym "t")) CIntTy))
    (CFunc () () ("a", CIntTy) (CIdent () "a"))

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
