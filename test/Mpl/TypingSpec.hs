module Mpl.TypingSpec where

import Test.Hspec

import Data.Text
import Control.Lens
import Mpl.Core (Core(..), Type(..), Path, metaOf)
import Mpl.Typing
  ( CaseFile
  , Evidence(..)
  , Bindings
  , investigate
  , bindingsFromList
  , conclusion
  , evidence
  , conflicts
  , identifierEvidence
  )
import qualified Data.Set        as Set
import qualified Data.Map.Strict as Map

data TestType = Concludes Type
              | ProvidesEvidence Evidence
              | ProvidesIdentEvidence Text Evidence
              | Conflicts Path

testWithBinding message boundIdents testType core = it message $ do
  let bindings = bindingsFromList boundIdents
      result   = investigate bindings core
      caseFile = metaOf result
  case testType of
    Conflicts path     -> Set.member path (view conflicts caseFile) `shouldBe` True
    Concludes ty       -> view conclusion caseFile `shouldBe` ty
    ProvidesEvidence e -> elem e (view evidence caseFile) `shouldBe` True
    ProvidesIdentEvidence name e ->
      case Map.lookup name (view identifierEvidence caseFile) of
        Nothing -> fail "No evidence"
        Just es -> elem e es `shouldBe` True

test message = testWithBinding message []

spec :: Spec
spec = do
  describe "investigate" $ do
    test "integer literal is of type int"
      (Concludes TInt)
      (CInt [0] () 1)

    test "integer literal is self evident"
      (ProvidesEvidence $ Literal [0] TInt)
      (CInt [0] () 1)

    test "unbound identifier is of type unbound identifier"
      (Concludes TUnboundIdent)
      (CIdent [0] () "a")

    testWithBinding "bound identifier awaits the type of its identifier"
      ["a"]
      (Concludes (TIdent "a"))
      (CIdent [0] () "a")

    test "thunk takes its body's type"
      (Concludes $ TThunk TInt)
      (CThunk [0] () (CInt [0, 0] () 1))

    testWithBinding "plus implies its argument is an integer"
      ["a"]
      (ProvidesIdentEvidence "a" $ ArgOf [0, 0] TInt)
      (CApp [0] ()
        (CIdent [0, 0] () "+")
        (CIdent [1, 0] () "a"))

    testWithBinding "plus implies its argument is an integer"
      ["a"]
      (ProvidesIdentEvidence "a" $ ArgOf [0, 0] TInt)
      (CApp [0] ()
        (CIdent [0, 0] () "+")
        (CIdent [1, 0] () "a"))

    test "plus is of type int -> int -> int"
      (Concludes $ TFunc TInt (TFunc TInt TInt))
      (CIdent [0] () "+")

    test "plus applied to an integer has type int -> int"
      (Concludes $ TFunc TInt TInt)
      (CApp [0] ()
        (CIdent [0, 0] () "+")
        (CInt   [1, 0] () 1))

    test "plus applied to two integers has type int"
      (Concludes TInt)
      (CApp [0] ()
        (CApp [0, 0] ()
          (CIdent [0, 0, 0] () "+")
          (CInt   [1, 0, 0] () 1))
        (CInt [1, 0] () 1))

    test "a lambda with no constraints on its parameter is polymorphic"
      (Concludes $ TFunc TPoly (TIdent "a"))
      (CFunc [0] () "a"
        (CIdent [0, 0] () "a"))

    test "a lambda with constraints on its parameter is monomorphic"
      (Concludes $ TFunc TInt TInt)
      (CFunc [0] () "a"
        (CApp [0, 0] ()
          (CApp [0, 0, 0] ()
            (CIdent [0, 0, 0, 0] () "+")
            (CIdent [1, 0, 0, 0] () "a"))
          (CIdent [1, 0, 0] () "a")))

    test "application of a polymorphic function always has the type of the function's body"
      (Concludes $ TInt)
      (CApp [0] ()
        (CFunc [0, 0] () "a"
          (CInt [0, 0, 0] () 1))
        (CInt [1, 0] () 1))

    test "application of a non-function is a conflict"
      (Conflicts [0])
      (CApp [0] ()
        (CInt [0, 0] () 1)
        (CInt [1, 0] () 1))

    test "forcing a thunk has the type of the thunk's body"
      (Concludes TInt)
      (CForce [0] ()
        (CThunk [0, 0] ()
          (CInt [0, 0, 0]  () 1)))

    test "forcing a non-thunk results in a conflict"
      (Conflicts [0])
      (CForce [0] ()
        (CInt [0, 0]  () 1))
