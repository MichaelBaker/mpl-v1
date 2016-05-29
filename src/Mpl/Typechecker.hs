module Mpl.Typechecker where

import Mpl.Parser     (AST(..))
import Data.Text.Read (signed, decimal, double)
import Data.Text      (Text)
import Data.List      (foldl')

import qualified Data.Text as T

data Type = IntType
          | FloatType
          | TextType
          | ListType Type
          | MapType Type Type
          | FuncType [Type] Type
          | Unknown
          deriving (Show, Eq)

data TAST = TInt   Integer
          | TFloat Double
          | TText  Text
          -- | TIdent  Text
          | TList  [Typed]
          | TMap   [(Typed, Typed)]
          -- | TFunc   TAST TAST
          -- | TApp    TAST [TAST]
         deriving (Show, Eq)

type Typed = (Type, TAST)

forceRead reader a = case reader a of
  Left e  -> error e
  Right b -> if T.null (snd b)
    then fst b
    else error ("Unconsumed input: " ++ (show $ snd b))

typecheck (AInt a)   = (IntType, TInt   $ forceRead (signed decimal) a)
typecheck (AFloat a) = (FloatType, TFloat $ forceRead double a)
typecheck (AText a)  = (TextType, TText  a)
typecheck (AList as) = typecheckList as
typecheck (AMap  as) = typecheckMap as
typecheck _          = undefined

typecheckMap as = (t, TMap tas)
  where tas = map (\(a, b) -> (typecheck a, typecheck b)) as
        t   = typeOfMap tas

typeOfMap []          = MapType Unknown Unknown
typeOfMap ((k, v):as) = MapType keyType valueType
  where (keyType, valueType) = foldl' checkHomogeneity (typeOf k, typeOf v) as
        checkHomogeneity (kt, vt) (k, v) = (newKt, newVt)
          where newKt = if kt /= Unknown && kt == typeOf k then kt else Unknown
                newVt = if vt /= Unknown && vt == typeOf v then vt else Unknown

typecheckList as = (t, TList tas)
  where tas = map typecheck as
        t   = typeOfList tas

typeOfList []       = ListType Unknown
typeOfList (ta:tas) = ListType $ foldl' checkHomogeneity (typeOf ta) tas
  where checkHomogeneity Unknown _ = Unknown
        checkHomogeneity t a       = if t == typeOf a then t else Unknown

typeOf (t, _) = t

