module Mpl.Typed.TypecheckSpec where

import           Mpl.Parser.SourceSpan
import           Mpl.ParserUtils
import           Mpl.Prelude
import           Mpl.Rendering.TypeError
import           Mpl.Typed.Core
import           Mpl.Typed.TestUtils
import           Mpl.Typed.Typecheck
import           Mpl.Utils
import           TestUtils
import qualified Data.List as List

spec = do
  describe "Inference" $ do
    it "infers integer literals to be integers" $ do
      "1" `infersTo` integer

    it "infers integer UTF8 strings to be strings" $ do
      "\"this is not a string\"" `infersTo` utf8String

    it "infers the type of a symbol from the context" $ do
      "a" `infersWithSetup` integer
        $ pushSymbol "a" (emptyAnnotation IntegerType)

    it "infers accurate type annotations" $ do
      "123: Integer" `infersTo` integer

    it "infers functions with annotated parameters" $ do
      "#(a: Integer = a)" `infersTo`
        function integer integer

    it "infers application" $ do
      "#(a: Integer = a) 1" `infersTo` integer
      "#(a: UTF8 = a) \"string\"" `infersTo` utf8String

    it "infers functions types" $ do
      "#(a: -> Integer Integer = a 1)" `infersTo`
        function (function integer integer) integer

  describe "Type errors" $ do
    it "rejects application of non-functions" $ do
      "1 1" `failsWith` (ApplicationOfNonFunction emptySpan (emptyAnnotation IntegerType))

    it "rejects arguments of the wrong type" $ do
      "#(a: Integer = a) #(a: Integer = a)" `failsWith`
        (InvalidArgument emptySpan (emptyAnnotation IntegerType) emptySpan (emptyAnnotation IntegerType))

    it "rejects unknown types" $ do
      "a: Wat" `failsWith` (UnboundTypeSymbol emptySpan "")

    it "rejects unbound symbols" $ do
      "a" `failsWith` (CannotInferSymbol emptySpan "")

    it "rejects incorrect type annotations" $ do
      "#(a: Integer = a): Integer" `failsWith`
        (InvalidTypeAnnotation (emptyAnnotation IntegerType) emptySpan (emptyAnnotation IntegerType) emptySpan)

    it "rejects over applied type functions" $ do
      "#(a: -> Integer Integer Integer = a 1)" `failsWith`
        (TooManyTypeArguments emptySpan [] [])

    it "rejects under applied type functions" $ do
      "#(a: -> Integer = a 1)" `failsWith`
        UnappliedTypeParameters emptySpan [] []

    it "rejects type level application of non functions" $ do
      "#(a: Integer Integer = a 1)" `failsWith`
        ApplicationOfNonTypeFunction emptySpan

  describe "Error messages" $ do
    it "renders UnboundTypeSymbol errors with suggestions" $ do
      "1: Intger" `containsError` "isn't defined"
      "1: Intger" `containsError` "Integer"
      "\"string\": UT8" `containsError` "UTF8"

    it "renders ApplicationOfNonFunction errors" $ do
      "(#(a: Integer = a) 1) 1" `containsError` "used as a function"

    it "renders CannotInferSymbol errors" $ do
      "a" `containsError` "isn't defined"
      "abb" `containsErrorWithSetup` "abc"
        $ (pushSymbol "abc" (emptyAnnotation IntegerType))

    it "renders Unimplemented errors" $ do
      "#(a = a)" `containsError` "unannotated parameters"

    it "renders InvalidTypeAnnotation errors" $ do
      "#(a: Integer = a): Integer" `containsError` "is different from its"

    it "renders InvalidArgument errors" $ do
      "#(a: Integer = a) #(a: Integer = a)" `containsError` "of the wrong"
      "#(a: Integer = a) \"string\"" `containsError` "of the wrong"

    it "renders TooManyTypeArguments errors" $ do
      "#(a: -> Integer Integer Integer = a 1)" `containsError`
        "too many arguments"

    it "renders UnappliedTypeParameters errors" $ do
      "#(a: -> Integer = a 1)" `containsError` "enough arguments"

    it "renders ApplicationOfNonTypeFunction errors" $ do
      "#(a: Integer Integer = a 1)" `containsError`
        "doesn't accept"

emptyAnnotation type_ = (emptySpan, NoReason) :< type_

infersTo code expectedType =
  infersWithSetup code expectedType (return ())

infersWithSetup code expectedType setup = expect $ do
  (bs, result) <- stringToCore code
  let checkResult = runTypecheck (setup >> infer result) standardContext
  return $ case checkResult of
    Left (e, context) ->
      expectationFailure (errorMessage bs context e)
    Right (ty, _) ->
      (cata Fix ty) `shouldBe` expectedType

failsWith code expectedType =
  failsWithSetup code expectedType (return ())

failsWithSetup code expected setup = expect $ do
  (_, result) <- stringToCore code
  let checkResult = runTypecheck (setup >> infer result) standardContext
  return $ case checkResult of
    Left e ->
      toConstr (fst e) `shouldBe` toConstr expected
    Right (ty, _) ->
      fail $ show ty ++ " typechecked successfully"

containsError code expected =
  containsErrorWithSetup code expected (return ())

containsErrorWithSetup code expected setup = expect $ do
  (bs, result) <- stringToCore code
  let checkResult = runTypecheck (setup >> infer result) standardContext
  return $ case checkResult of
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
    Right (ty, _) ->
      fail $ show ty ++ " typechecked successfully"

integer =
  Fix IntegerType

utf8String =
  Fix UTF8StringType

function a b =
  Fix (FunctionType a b)
