module Mpl.Common.CoreSpec where

import Data.Functor.Foldable (Fix(..))
import Mpl.Common.Parsing    (parseExpressionText)
import Mpl.Utils             (cata, envcata)
import TestUtils             (describe, it, shouldBe, mkTransformsTo)

import qualified Mpl.Common.Syntax as S
import qualified Mpl.Common.Core   as C

transformsTo = mkTransformsTo
  parseExpressionText
  (Fix . C.mapBinder (cata Fix))
  (envcata (C.syntaxToCore id))

int :: Integer -> Fix (C.CoreF (Fix C.Binder))
int              = Fix . C.int
binder           = Fix . C.binder
symbol           = Fix . C.symbol
application a b  = Fix $ C.application a b
function a b     = Fix $ C.function a b

spec = do
  it "converts integers" $ do
    "1" `transformsTo` (int 1)

  it "converts symbols" $ do
    "a" `transformsTo` (symbol "a")

  it "converts functions of one parameter" $ do
    "#(a = a)" `transformsTo`
      (function (binder "a") (symbol "a"))

  it "curries functions of multiple parameters" $ do
    "#(a b c = a)" `transformsTo`
      (function (binder "a")
        (function (binder "b")
          (function (binder "c") (symbol "a"))))

  it "converts application of one argument" $ do
    "f 1" `transformsTo` (application (symbol "f") (int 1))

  it "curries application of multiple arguments" $ do
    "f 1 2 3" `transformsTo`
      (application
        (application
          (application
            (symbol "f")
            (int 1))
          (int 2))
        (int 3))
