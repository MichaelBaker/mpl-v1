module Mpl.Typed.TestUtils where

import           Mpl.ParserUtils
import           Mpl.Prelude
import           Mpl.Utils
import           Test.Hspec
import qualified Mpl.Rendering.ParserError as ParserError
import qualified Mpl.Typed.Core            as C
import qualified Mpl.Typed.Parsing         as Parsing
import qualified Mpl.Typed.Syntax          as S
import qualified Mpl.Typed.SyntaxToCore    as SyntaxToCore

type SourceCore =
  SourceAnnotated (C.CoreF SourceCoreType SourceCoreBinder)

type SourceCoreBinder =
  SourceAnnotated (C.Binder SourceCoreType)

type SourceCoreType =
  SourceAnnotated C.Type

stringToSyntax :: String -> Either Expectation (ByteString, Parsing.AnnotatedSyntax)
stringToSyntax code =
  case Parsing.parseString code of
    (bs, Left e) ->
      Left $ expectationFailure (ParserError.errorMessage bs e)
    (bs, Right result) -> do
      Right (bs, result)

stringToCore :: String -> Either Expectation (ByteString, SourceCore)
stringToCore code = do
  (bs, syntax) <- stringToSyntax code
  syntax
    |> envcata SyntaxToCore.transform
    |> ( run
       . SyntaxToCore.runTransform
       . SyntaxToCore.runTransformBinder
       )
    |> ((,) bs)
    |> Right
