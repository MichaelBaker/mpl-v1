module Mpl.Typed.TypecheckSpec where

import           Mpl.ParserUtils
import           Mpl.Prelude
import           Mpl.Rendering.TypeError
import           Mpl.Typed.TestUtils
import           Mpl.Typed.Typecheck
import           TestUtils
import qualified Data.List as List

spec = do
  describe "Inference" $ do
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

    it "infers application" $ do
      "#(a: Integer = a) 1" `infersTo` IntegerType

  describe "Type errors" $ do
    it "rejects application of non-functions" $ do
      "1 1" `failsWith` (ApplicationOfNonFunction emptySpan "")

    it "rejects arguments of the wrong type" $ do
      "#(a: Integer = a) #(a: Integer = a)" `failsWith`
        (InvalidArgument "")

    it "rejects unknown types" $ do
      "a: Wat" `failsWith` (UnboundTypeSymbol emptySpan "")

    it "rejects unbound symbols" $ do
      "a" `failsWith` (CannotInferSymbol "")

    it "rejects incorrect type annotations" $ do
      "#(a: Integer = a): Integer" `failsWith`
        (InvalidTypeAnnotation "")

  describe "Error messages" $ do
    it "renders UnboundTypeSymbol errors with suggestions" $ do
      "1: Intger" `containsError` "isn't defined"
      "1: Intger" `containsError` "Integer"

    it "renders ApplicationOfNonFunction errors with suggestions" $ do
      "(#(a: Integer = a) 1) 1" `containsError` "used as a function"

infersTo code expectedType =
  infersWithSetup code expectedType (return ())

infersWithSetup code expectedType setup =
  case snd $ textToCore code of
    Left e ->
      fail $ show e
    Right a ->
      case runTypecheck (setup >> infer a) standardContext of
        Left e ->
          fail $ show (fst e)
        Right a ->
          (fst a) `shouldBe` expectedType

failsWith code expectedType =
  failsWithSetup code expectedType (return ())

failsWithSetup code expected setup =
  case snd $ textToCore code of
    Left e ->
      fail $ show e
    Right a ->
      case runTypecheck (setup >> infer a) standardContext of
        Left e ->
          normalizeError (fst e) `shouldBe` normalizeError expected
        Right a ->
          fail $ show (fst a) ++ " typechecked successfully"

containsError code expected =
  containsErrorWithSetup code expected (return ())

containsErrorWithSetup code expected setup =
  case textToCore code of
    (_, Left e) ->
      fail $ show e
    (bs, Right a) ->
      case runTypecheck (setup >> infer a) standardContext of
        Left (e, context) -> do
          let message = errorMessage bs context e
          if List.isInfixOf expected message
            then
              return ()
            else do
              expectationFailure $ concat
                [ "\n"
                , "==== Expected " ++ show code ++ " to contain the string:\n\n"
                , expected
                , "\n\n"
                , "==== This was the error that was produced:\n\n"
                , message
                , "\n"
                ]
        Right ty ->
          fail $ show (fst ty) ++ " typechecked successfully"

normalizeError (UnimplementedError _) =
  UnimplementedError ""

normalizeError (UnboundTypeSymbol _ _) =
  UnboundTypeSymbol emptySpan ""

normalizeError (CannotInferSymbol _) =
  CannotInferSymbol ""

normalizeError (ApplicationOfNonFunction _ _) =
  ApplicationOfNonFunction emptySpan ""

normalizeError (InvalidArgument _) =
  InvalidArgument ""

normalizeError (InvalidTypeAnnotation _) =
  InvalidTypeAnnotation ""

