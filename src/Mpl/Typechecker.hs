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
          | TApp   Typed [Typed]
         deriving (Show, Eq)

type Typed   = (Type, TAST)
type Context = Map.Map Text Type

typecheck = fst . (typecheckWithContext emptyContext)

typecheckWithContext ctx (AInt _ a)             = ((IntType, TInt   $ forceRead (signed decimal) a), ctx)
typecheckWithContext ctx (AFloat _ a)           = ((FloatType, TFloat $ forceRead double a), ctx)
typecheckWithContext ctx (AText _ a)            = ((TextType, TText  a), ctx)
typecheckWithContext ctx (AIdent _ a)           = ((typeOfIdent a ctx, TIdent a), ctx)
typecheckWithContext ctx (AList _ as)           = (typecheckList as, ctx)
typecheckWithContext ctx (AMap  _ as)           = (typecheckMap as, ctx)
typecheckWithContext ctx (AFunc _ (AList _ as) a) = typecheckFunc ctx as a
typecheckWithContext ctx (AApp _ (AFunc _ (AList _ ps) b) as) = typecheckApp ctx ps b as
typecheckWithContext ctx (AFunc _ ps b)         = error $ "Invalid function AST: " ++ show (AFunc () ps b)
typecheckWithContext ctx (AApp _ f as)          = error $ "Invalid application AST: " ++ show (AApp () f as)

forceRead reader a = case reader a of
  Left e  -> error e
  Right b -> if T.null (snd b)
    then fst b
    else error ("Unconsumed input: " ++ (show $ snd b))

emptyContext = Map.empty :: Context

typeOfIdent i context = case Map.lookup i context of
  Nothing -> Unknown
  Just a  -> a

typecheckApp ctx params body args = if numParams == numArgs
  then case typecheckFunc (Map.union argctx ctx) params body of
    ((FuncType pts bt, f), newCtx) -> ((bt, (TApp (FuncType pts bt, f) targs)), newCtx)
    _ -> error $ "Failed to typecheck application: " ++ show (params, body, args)
  else error $ "Function expects " ++ show numParams ++ " parameters, but " ++ show numArgs ++ " arguments are supplied"
  where numParams     = length params
        numArgs       = length args
        (targs, ctxs) = unzip $ map (typecheckWithContext ctx) args
        argctx        = foldl' addArg emptyContext $ zip params $ map fst targs
        addArg ctx (AIdent _ p, Unknown) = ctx
        addArg ctx (AIdent _ p, t)       = Map.insert p t ctx
        addArg _ _ = error $ "Invalid application AST: " ++ show (AApp () (AFunc () (AList () params) body) args)

typecheckFunc ctx params body = ((t, tast), newContext)
  where t                = FuncType (reverse paramTypes) (typeOf tbody)
        tast             = TFunc (ListType IdentType, TList (reverse tparams)) tbody
        (tbody, context) = typecheckWithContext ctx body
        (paramTypes, tparams, newContext) = foldl' typeOfParam ([], [], context) params
        typeOfParam (pts, tps, ctx) (AIdent _ a) = (newPt:pts, newTp:tps, newCtx)
          where newPt  = typeOfIdent a ctx
                newTp  = (IdentType, TIdent a)
                newCtx = Map.delete a ctx
        typeOfParam _ _ = error $ "Invalid function AST: " ++ show (AFunc () (AList () params) body)

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
