module Mpl.Typechecker where

import Mpl.Parser     (AST(..))
import Data.Text.Read (signed, decimal, double)
import Data.Text      (Text)
import Data.List      (foldl')

import qualified Data.Map.Strict as Map
import qualified Data.Text       as T

data Type = IntType
          | FloatType
          | TextType
          | IdentType
          | ListType Type
          | MapType  Type Type
          | FuncType [Type] Type
          | Unknown
          deriving (Show, Eq)

data TAST = TInt   Integer
          | TFloat Double
          | TText  Text
          | TIdent Text
          | TList  [Typed]
          | TMap   [(Typed, Typed)]
          | TFunc  Typed Typed
          -- | TApp    TAST [TAST]
         deriving (Show, Eq)

type Typed   = (Type, TAST)
type Context = Map.Map Text Type

typecheck = fst . typecheckWithContext

typecheckWithContext (AInt a)             = ((IntType, TInt   $ forceRead (signed decimal) a), emptyContext)
typecheckWithContext (AFloat a)           = ((FloatType, TFloat $ forceRead double a), emptyContext)
typecheckWithContext (AText a)            = ((TextType, TText  a), emptyContext)
typecheckWithContext (AIdent a)           = ((Unknown, TIdent a), emptyContext)
typecheckWithContext (AList as)           = (typecheckList as, emptyContext)
typecheckWithContext (AMap  as)           = (typecheckMap as, emptyContext)
typecheckWithContext (AFunc (AList as) a) = typecheckFunc as a
typecheckWithContext (AFunc as a)         = error $ "Invalid function AST: " ++ show (AFunc as a)
typecheckWithContext _ = undefined

forceRead reader a = case reader a of
  Left e  -> error e
  Right b -> if T.null (snd b)
    then fst b
    else error ("Unconsumed input: " ++ (show $ snd b))

emptyContext = Map.empty :: Context

typeOfIdent i context = case Map.lookup i context of
  Nothing -> Unknown
  Just a  -> a

typecheckFunc params body = ((t, tast), newContext)
  where t                = FuncType (reverse paramTypes) (typeOf tbody)
        tast             = TFunc (ListType IdentType, TList (reverse tparams)) tbody
        (tbody, context) = typecheckWithContext body
        (paramTypes, tparams, newContext) = foldl' typeOfParam ([], [], context) params
        typeOfParam (pts, tps, ctx) (AIdent a) = (newPt:pts, newTp:tps, newCtx)
          where newPt  = typeOfIdent a ctx
                newTp  = (IdentType, TIdent a)
                newCtx = Map.delete a ctx
        typeOfParam _ _ = error $ "Invalid function AST: " ++ show (AFunc (AList params) body)

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
