module Mpl.Typed.TestUtils where

import           Mpl.Prelude
import           Mpl.ParserUtils
import           Mpl.Utils
import           Mpl.Typed.Parsing
import qualified Mpl.Typed.SyntaxToCore as SyntaxToCore
import qualified Mpl.Typed.Core         as C

textToCore :: Text -> (ByteString, Either String (SourceAnnotated (C.CoreF (SourceAnnotated C.Binder))))
textToCore text =
  case parseExpressionText text of
    (bs, Left e) ->
      (bs, Left $ show e)
    (bs, Right syntax) ->
      syntax
      |> envcata SyntaxToCore.transform
      |> ( run
         . SyntaxToCore.runTransform
         . SyntaxToCore.runTransformBinder
         )
      |> Right
      |> ((,) bs)
