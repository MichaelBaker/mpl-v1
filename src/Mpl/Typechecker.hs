module Mpl.Typechecker where

import Mpl.AST (AST(..), Type(..), meta)

import Data.Text (Text)
import Data.List (foldl')

import qualified Data.Map.Strict as Map

type Context = Map.Map Text Type

typecheck = fst . typecheckWithContext emptyContext

typecheckWithContext :: Context -> AST () -> (AST Type, Context)
typecheckWithContext ctx (AInt _ a)       = (AInt IntType a, ctx)
typecheckWithContext ctx (AFloat _ a)     = (AFloat FloatType a, ctx)
typecheckWithContext ctx (AText _ a)      = (AText TextType a, ctx)
typecheckWithContext ctx (AIdent _ a)     = (AIdent (typeOfIdent a ctx) a, ctx)
typecheckWithContext ctx (AList _ as)     = (typecheckList as, ctx)
typecheckWithContext ctx (AMap  _ as)     = (typecheckMap as, ctx)
typecheckWithContext ctx f@(ACFunc _ p a) = typecheckCFunc ctx Nothing f
typecheckWithContext ctx (ACApp _ f a)    = typecheckApp ctx f a
typecheckWithContext ctx f@(AFunc _ _ _)  = error $ "Invalid uncurried function: " ++ show f
typecheckWithContext ctx a@(AApp _ _ _)   = error $ "Invalid application AST: " ++ show a

emptyContext = Map.empty :: Context

typeOfIdent i context = case Map.lookup i context of
  Nothing -> Unknown
  Just a  -> a

typecheckApp ctx f arg = case arg of
  Nothing -> let (tf, newCtx) = typecheckCFunc ctx Nothing f in (ACApp (meta tf) tf Nothing, newCtx)
  Just a  -> let (targ, argCtx) = typecheckWithContext ctx a
                 (tast, newCtx) = typecheckCFunc argCtx (Just (meta targ)) f
               in case meta tast of
                 FuncType a b -> if meta targ == a
                                   then (ACApp b tast (Just targ), newCtx)
                                   else (ACApp Unknown tast (Just targ), newCtx)
                 _            -> (ACApp Unknown tast (Just targ), newCtx)

typecheckCFunc ctx (Just a) (ACFunc _ (Just p) body) =
  let (tbody, newContext) = typecheckWithContext (Map.insert p a ctx) body
    in (ACFunc (FuncType a (meta tbody)) (Just p) tbody, newContext)
typecheckCFunc ctx _ (ACFunc _ Nothing body) =
  let (tbody, newContext) = typecheckWithContext ctx body
    in (ACFunc (meta tbody) Nothing tbody, newContext)
typecheckCFunc ctx _ (ACFunc _ (Just p) body) =
  let (tbody, newContext) = typecheckWithContext ctx body
    in (ACFunc (FuncType (typeOfIdent p newContext) (meta tbody)) (Just p) tbody, newContext)
typecheckCFunc ctx a (ACApp _ f Nothing) =
  typecheckCFunc ctx a f
typecheckCFunc ctx _ a = typecheckWithContext ctx a

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
