module Mpl.Interpreter where

import Mpl.Parser   (AST(..), parse)
import Mpl.Object   (Object(..), Type(..))
import Data.Text    (Text, pack, unpack)
import Text.Earley  (Report)
import Data.Dynamic (toDyn)

eval :: String -> String
eval = show . exec . handleParseFail . parse . pack

exec :: AST -> Object
exec (App a b) = Object TError $ toDyn ("Function application not implemented yet" :: String)
exec (Ident a) = Object TError $ toDyn ("Identifiers not implemented yet" :: String)
exec (Int a)   = Object TInt   $ toDyn (read $ unpack a :: Integer)
exec (Float a) = Object TFloat $ toDyn (read $ unpack a :: Float)

handleParseFail :: ([AST], Report Text Text) -> AST
handleParseFail (a:_, _) = a
handleParseFail (_, r)   = error $ show r
