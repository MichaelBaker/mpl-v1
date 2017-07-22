module Mpl.Rendering.TypeError where

import qualified Data.List                    as List
import qualified Data.Text                    as Text
import           Data.Text.Metrics
import           Mpl.Parser.SourceSpan
import           Mpl.ParserUtils
import           Mpl.Prelude
import           Mpl.Rendering
import           Mpl.Typed.Core
import           Mpl.Typed.Typecheck
import           Mpl.Utils
import qualified Text.PrettyPrint.ANSI.Leijen as P

errorMessage :: ByteString -> Context -> TypeError -> String
errorMessage byteString context error =
  render
    $   (header_ "Type Error")
    <~> blankLine
    <~> message byteString context error

message byteString context (UnboundTypeSymbol span symbol) =
  unboundTypeSymbol byteString context span symbol

message byteString context (ApplicationOfNonFunction span type_) =
  applicationOfNonFunction byteString context span type_

message byteString context (CannotInferSymbol span symbol) =
  cannotInferSymbol byteString context span symbol

message byteString context (Unimplemented span explanation) =
      toDoc explanation
  <~> blankLine
  <~> indent (highlight byteString span problem)

message byteString context (InvalidTypeAnnotation inferredType inferredSpan annotatedType annotationSpan) =
  invalidTypeAnnotation byteString context inferredType inferredSpan annotatedType annotationSpan

message byteString context (InvalidArgument functionSpan paramType argumentSpan argumentType) =
  invalidArgument byteString context functionSpan paramType argumentSpan argumentType

message byteString context (TooManyTypeArguments constructorSpan successfulArgs extraArgs) =
  tooManyTypeArguments byteString context constructorSpan successfulArgs extraArgs

message byteString context (UnappliedTypeParameters constructorSpan successfulArgs extraParams) =
  unappliedTypeParameters byteString context constructorSpan successfulArgs extraParams

message byteString context (ApplicationOfNonTypeFunction constructorSpan) =
  applicationOfNonTypeFunction byteString context constructorSpan

cannotInferSymbol byteString context span symbol =
      "The symbol " <~> problem symbol <~> " isn't defined in the following expression."
  <~> blankLine
  <~> indent (highlight byteString span problem)
  <~> suggestions
  where possibleSymbols =
          List.map fst (symbolTypes context) |> List.filter ((< 3) . levenshtein symbol)

        suggestions =
          if not (List.null possibleSymbols)
            then
                  blankLine
              <~> "Here are some defined symbols that are similar to " <~> toDoc symbol <~> ":"
              <~> blankLine
              <~> indent (stack $ fmap toDoc possibleSymbols)
            else
              ""

unboundTypeSymbol byteString context span symbol =
      "The type " <~> problem symbol <~> " isn't defined in the following type annotation."
  <~> blankLine
  <~> indent (highlight byteString span problem)
  <~> suggestions
  where possibleTypes =
          typeSymbolTypes context
          |> fmap fst
          |> List.filter ((< 3) . levenshtein symbol)

        suggestions =
          if not (List.null possibleTypes)
            then
                  blankLine
              <~> "Here are some defined types that are similar to " <~> toDoc symbol <~> ":"
              <~> blankLine
              <~> indent (stack $ fmap toDoc possibleTypes)
            else
              ""

applicationOfNonFunction byteString context span type_ =
      "The following value is being used as a function"
  <~> blankLine
  <~> indent (highlight byteString span problem)
  <~> blankLine
  <~> "but it has type"
  <~> blankLine
  <~> indent (problem type_)
  <~> blankLine
  <~> "It has type"
  <~> hardline
  <~> indent (callout type_)
  <~> hardline
  <~> "because "
  <~> renderReason byteString type_

invalidTypeAnnotation byteString context inferredType inferredSpan annotatedType annotationSpan =
      "The inferred type of the following " <~> callout_ "expression" <~> " is different from its " <~> problem_ "annotation" <~> "."
  <~> blankLine
  <~> indent
    (   toDoc (upTo (startDelta inferredSpan) byteString)
    <~> callout (between (startDelta inferredSpan) (endDelta inferredSpan) byteString)
    <~> toDoc (betweenExclusive (endDelta inferredSpan) (startDelta annotationSpan) byteString)
    <~> problem (between (startDelta annotationSpan) (endDelta annotationSpan) byteString)
    <~> toDoc (after (endDelta annotationSpan) byteString)
    )
  <~> blankLine
  <~> "The expression's inferred type is:"
  <~> hardline
  <~> indent (callout inferredType)
  <~> blankLine
  <~> "Its annotated type is:"
  <~> hardline
  <~> indent (problem annotatedType)

invalidArgument byteString context functionSpan paramType argumentSpan argumentType =
      "The following " <~> callout_ "function" <~> " is being applied to an " <~> problem_ "argument" <~> " of the wrong type."
  <~> blankLine
  <~> indent
    (   toDoc (upTo (startDelta functionSpan) byteString)
    <~> callout (between (startDelta functionSpan) (endDelta functionSpan) byteString)
    <~> toDoc (betweenExclusive (endDelta functionSpan) (startDelta argumentSpan) byteString)
    <~> problem (between (startDelta argumentSpan) (endDelta argumentSpan) byteString)
    <~> toDoc (after (endDelta argumentSpan) byteString)
    )
  <~> blankLine
  <~> "The function takes a parameter of type:"
  <~> hardline
  <~> indent (callout paramType)
  <~> blankLine
  <~> "The argument is of type:"
  <~> hardline
  <~> indent (problem argumentType)
  <~> hardline
  <~> blankLine
  <~> indent
    (   "The parameter has type"
    <~> hardline
    <~> indent (callout paramType)
    <~> hardline
    <~> "because "
    <~> renderReason byteString paramType
    <~> hardline
    <~> blankLine
    <~> "The argument has type"
    <~> hardline
    <~> indent (callout argumentType)
    <~> hardline
    <~> "because "
    <~> renderReason byteString argumentType
    )

tooManyTypeArguments byteString context constructorSpan successfulArgs extraArgs =
      "The type constructor \"" <~> problem (extractSpan constructorSpan byteString) <~> "\" is applied to too many arguments. It takes " <~> callout (successfulArgCount) <~> " but it was applied to " <~> problem totalArgs <~> "."
  <~> blankLine
  <~> indent (toDoc byteString)
  where successfulArgCount = List.length successfulArgs
        totalArgs = List.length extraArgs + successfulArgCount

unappliedTypeParameters byteString context constructorSpan successfulArgs extraParams =
      "The type constructor \"" <~> problem (extractSpan constructorSpan byteString) <~> "\" isn't applied to too enough arguments. It takes " <~> callout (totalArgs) <~> " but it was only applied to " <~> problem successfulArgCount <~> "."
  <~> blankLine
  <~> indent (toDoc byteString)
  where successfulArgCount = List.length successfulArgs
        totalArgs = List.length extraParams + successfulArgCount

applicationOfNonTypeFunction byteString context constructorSpan =
      "The type constructor \"" <~> problem (extractSpan constructorSpan byteString) <~> "\" doesn't accept any arguments."
  <~> blankLine
  <~> indent (toDoc byteString)

renderReason byteString ((_, InferredIntegerLiteral span) :< _) =
  "it is an integer literal"
  <~> hardline
  <~> indent (pretty $ extractSpan span byteString)

renderReason byteString ((_, InferredUTF8StringLiteral span) :< _) =
  "it is a UTF8 string literal"
  <~> hardline
  <~> indent (pretty $ extractSpan span byteString)

renderReason byteString ((_, _) :< FunctionType paramType bodyType) =
      "the function's body has type"
  <~> hardline
  <~> indent (callout bodyType)
  <~> hardline
  <~> "because " <~> renderReason byteString bodyType
  <~> hardline
  <~> "and the function's parameter has type"
  <~> hardline
  <~> indent (callout paramType)
  <~> hardline
  <~> "because " <~> renderReason byteString paramType

renderReason byteString ((originalSpan, InferredSymbolType span reason) :< type_) =
      "the symbol \"" <~> callout (extractSpan span byteString) <~> "\" has type"
  <~> hardline
  <~> indent (callout type_)
  <~> hardline
  <~> "because " <~> renderReason byteString ((originalSpan, reason) :< type_)

renderReason byteString ((_, InferredFromTypeAnnotation span) :< _) =
      "of a type annotation"
  <~> hardline
  <~> indent (pretty $ extractSpan span byteString)

renderReason _ _ =
  toDoc ("hello" :: Text)

instance (P.Pretty a) => P.Pretty (Type a) where
  pretty IntegerType =
    text "Integer"

  pretty UTF8StringType =
    text "UTF8"

  pretty (TypeSymbol symbol) =
    text symbol

  pretty (FunctionType parameter body) =
    P.encloseSep "(" ")" " " ["->", pretty parameter, pretty body]

  pretty (TypeApplication function argument) =
    P.encloseSep "(" ")" " " [pretty function, pretty argument]
