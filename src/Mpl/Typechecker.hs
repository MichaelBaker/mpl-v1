module Mpl.Typechecker where

import Mpl.AST (AST(..), meta)

import Data.Text      (Text)
import Data.List      (foldl')

import qualified Data.Map.Strict as Map

data Type = IntType
          | FloatType
          | TextType
          | IdentType
          | ListType Type
          | MapType  Type Type
          | FuncType [Type] Type
          | Unknown
          deriving (Show, Eq)

type TAST    = AST Type
type Context = Map.Map Text Type

typecheck = fst . (typecheckWithContext emptyContext)

typecheckWithContext ctx (AInt _ a)             = (AInt IntType a, ctx)
typecheckWithContext ctx (AFloat _ a)           = (AFloat FloatType a, ctx)
typecheckWithContext ctx (AText _ a)            = (AText TextType a, ctx)
typecheckWithContext ctx (AIdent _ a)           = (AIdent (typeOfIdent a ctx) a, ctx)
typecheckWithContext ctx (AList _ as)           = (typecheckList as, ctx)
typecheckWithContext ctx (AMap  _ as)           = (typecheckMap as, ctx)
typecheckWithContext ctx (AFunc _ (AList _ as) a) = typecheckFunc ctx as a
typecheckWithContext ctx (AApp _ (AFunc _ (AList _ ps) b) as) = typecheckApp ctx ps b as
typecheckWithContext ctx (AFunc _ ps b)         = error $ "Invalid function AST: " ++ show (AFunc () ps b)
typecheckWithContext ctx (AApp _ f as)          = error $ "Invalid application AST: " ++ show (AApp () f as)

emptyContext = Map.empty :: Context

typeOfIdent i context = case Map.lookup i context of
  Nothing -> Unknown
  Just a  -> a

typecheckApp ctx params body args = if numParams == numArgs
  then case typecheckFunc (Map.union argctx ctx) params body of
    (AFunc (FuncType pts bt) ps b, newCtx) -> (AApp bt (AFunc (FuncType pts bt) ps b) targs, newCtx)
    _ -> error $ "Failed to typecheck application: " ++ show (params, body, args)
  else error $ "Function expects " ++ show numParams ++ " parameters, but " ++ show numArgs ++ " arguments are supplied"
  where numParams     = length params
        numArgs       = length args
        (targs, ctxs) = unzip $ map (typecheckWithContext ctx) args
        argctx        = foldl' addArg emptyContext $ zip params $ map meta targs
        addArg ctx (AIdent _ p, Unknown) = ctx
        addArg ctx (AIdent _ p, t)       = Map.insert p t ctx
        addArg _ _ = error $ "Invalid application AST: " ++ show (AApp () (AFunc () (AList () params) body) args)

typecheckFunc ctx params body = (tast, newContext)
  where t                = FuncType (reverse paramTypes) (meta tbody)
        tast             = AFunc t (AList (ListType IdentType) (reverse tparams)) tbody
        (tbody, context) = typecheckWithContext ctx body
        (paramTypes, tparams, newContext) = foldl' typeOfParam ([], [], context) params
        typeOfParam (pts, tps, ctx) (AIdent _ a) = (newPt:pts, newTp:tps, newCtx)
          where newPt  = typeOfIdent a ctx
                newTp  = AIdent IdentType a
                newCtx = Map.delete a ctx
        typeOfParam _ _ = error $ "Invalid function AST: " ++ show (AFunc () (AList () params) body)

typecheckMap as = AMap t tas
  where tas = map (\(a, b) -> (typecheck a, typecheck b)) as
        t   = typeOfMap tas

typeOfMap []          = MapType Unknown Unknown
typeOfMap ((k, v):as) = MapType keyType valueType
  where (keyType, valueType) = foldl' checkHomogeneity (meta k, meta v) as
        checkHomogeneity (kt, vt) (k, v) = (newKt, newVt)
          where newKt = if kt /= Unknown && kt == meta k then kt else Unknown
                newVt = if vt /= Unknown && vt == meta v then vt else Unknown

typecheckList as = AList t tas
  where tas = map typecheck as
        t   = typeOfList tas

typeOfList []       = ListType Unknown
typeOfList (ta:tas) = ListType $ foldl' checkHomogeneity (meta ta) tas
  where checkHomogeneity Unknown _ = Unknown
        checkHomogeneity t a       = if t == meta a then t else Unknown
