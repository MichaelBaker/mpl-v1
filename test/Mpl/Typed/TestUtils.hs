module Mpl.Typed.TestUtils where

import           Mpl.Prelude
import           Mpl.ParserUtils
import           Mpl.Utils
import           Mpl.Typed.Parsing
import qualified Mpl.Typed.SyntaxToCore as SyntaxToCore
import qualified Mpl.Typed.Core         as C

textToCore :: Text -> Either String (SourceAnnotated (C.CoreF (SourceAnnotated C.Binder)))
textToCore text =
  case snd $ parseExpressionText text of
    Left e ->
      Left $ show e
    Right syntax ->
      syntax
      |> envcata SyntaxToCore.transform
      |> ( run
         . SyntaxToCore.runTransform
         . SyntaxToCore.runTransformBinder
         )
      |> Right
