module Mpl.ASTToCoreSpec where

import Test.Hspec
import ASTHelpers (aparen, asquare, acurly, aint, afloat, atext, asym)

import Mpl.AST       (AST(..), Core(..), CoreType(..))
import Mpl.ASTToCore (astToCore)

test message ast core = it message (astToCore () ast `shouldBe` core)

param name ty = aparen [asym ":", asym name, asym ty]

spec :: Spec
spec = do
  test "int   -> int"   (aint 8)   (CInt [0] 8)
  test "float -> real"  (afloat 8.0) (CReal  [0] 8.0)
  test "text  -> text"  (atext "a") (CText  [0] "a")
  test "ident -> ident" (asym "a") (CIdent [0] "a")

  test "empty parens -> unit"
    (aparen [])
    (CUnit [0])

  test "square brackets -> list"
    (asquare [])
    (CList [0] [])

  test "curly brackets -> assoc"
    (acurly [])
    (CAssoc [0] [])

  test "a function with no arguments to a thunk"
    (aparen [asym "#", asquare [], aparen []])
    (CThunk [0] () (CUnit [0, 0]))

  test "a type function with no arguments to a thunk"
    (aparen [asym ":", asquare [], aparen []])
    (CThunk [0] () (CUnit [0, 0]))

  test "a function of one argument"
    (aparen [asym "#", asquare [param "a" "t"], aparen []])
    (CFunc [0] () ("a", "t") (CUnit [0, 0]))

  test "curries a function of multiple arguments"
    (aparen [asym "#", asquare [param "a" "t", param "b" "s"], aparen []])
    (CFunc [0] () ("a", "t")
      (CFunc [0, 0] () ("b", "s")
        (CUnit [0, 0, 0])))

  test "one type argument"
    (aparen [asym ":", asquare [asym "t"], (aparen [asym "#", asquare [param "a" "t"], aint 5])])
    (CTyFunc [0] () "t"
      (CFunc [0, 0] () ("a", "t")
        (CInt [0, 0, 0] 5)))

  test "curries multiple type arguments"
    (aparen [asym ":", asquare [asym "t", asym "s"], (aparen [asym "#", asquare [param "a" "t", param "b" "s"], aint 5])])
    (CTyFunc [0] () "t"
      (CTyFunc [0, 0] () "s"
        (CFunc [0, 0, 0] () ("a", "t")
          (CFunc [0, 0, 0, 0] () ("b", "s")
            (CInt [0, 0, 0, 0, 0] 5)))))

  test "an application with no arguments as a force"
    (aparen [aparen []])
    (CForce [0] (CUnit [0, 0]))

  test "application of a single argument"
    (aparen [aparen [], aparen []])
    (CApp [0] (CUnit [0, 0]) (CUnit [1, 0]))

  test "curries application of multiple arguments"
    (aparen [aparen [], aint 1, aint 2])
    (CApp [0]
      (CApp [0, 0]
        (CUnit [0, 0, 0])
        (CInt  [1, 0, 0] 1))
      (CInt [1, 0] 2))
