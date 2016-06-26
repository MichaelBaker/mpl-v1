module Mpl.TypingSpec where

import Test.Hspec

import Control.Lens
import Mpl.Core (Core(..), Type(..), metaOf)
import Mpl.Typing
  ( CaseFile
  , Evidence(..)
  , Bindings
  , investigate
  , bindingsFromList
  , conclusion
  , evidence
  )

data TestType = Concludes Type
              | ProvidesEvidence Evidence

testWithBinding message boundIdents testType core = it message $ do
  let bindings = bindingsFromList boundIdents
      result   = investigate bindings core
      caseFile = metaOf result
  case testType of
    Concludes ty       -> view conclusion caseFile `shouldBe` ty
    ProvidesEvidence e -> elem e (view evidence caseFile) `shouldBe` True

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

    testWithBinding "bound identifier is of type poly"
      ["a"]
      (Concludes TPoly)
      (CIdent [0] () "a")

    test "thunk takes its body's type"
      (Concludes $ TThunk TInt)
      (CThunk [0] () (CInt [0, 0] () 1))
