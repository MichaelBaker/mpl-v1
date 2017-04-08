module Mpl.Typed.TypecheckSpec where

import Mpl.Typed.Parsing   (parseExpressionText)
import Mpl.Typed.Typecheck (Type(..), infer, eval, standardContext, addSymbol)
import TestUtils           (it, shouldBe)

infersTo code expectedType =
  infersWithSetup code expectedType (return ())

infersWithSetup code expectedType setup =
  case snd (parseExpressionText code) of
    Left e ->
      fail $ show e
    Right a ->
      case eval (setup >> infer a) standardContext of
        Left e ->
          fail $ show e
        Right ty ->
          ty `shouldBe` expectedType

spec = do
  it "infers integer literals to be integers" $ do
    "1" `infersTo` IntegerType

  it "infers the type of a symbol from the context" $ do
    "a" `infersWithSetup` IntegerType
      $ addSymbol "a" IntegerType

  it "infers accurate type annotations" $ do
    "123: Integer" `infersTo` IntegerType
