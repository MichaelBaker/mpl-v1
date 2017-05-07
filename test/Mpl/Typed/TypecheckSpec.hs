module Mpl.Typed.TypecheckSpec where

import Mpl.Prelude
import Mpl.Typed.TestUtils
import Mpl.Typed.Typecheck
import TestUtils

infersTo code expectedType =
  infersWithSetup code expectedType (return ())

infersWithSetup code expectedType setup =
  case textToCore code of
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
      $ pushSymbol "a" IntegerType

  it "infers accurate type annotations" $ do
    "123: Integer" `infersTo` IntegerType

  it "infers functions with annotated parameters" $ do
    "#(a: Integer = a)" `infersTo`
      FunctionType IntegerType IntegerType
