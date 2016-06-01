module Mpl.TypeAnnotation where

import Data.Text (Text)
import Mpl.AST   (Core(..), CoreType(..), meta)

import qualified Data.Map.Strict as Map

type Context = Map.Map Text CoreType

annotate = annotate' Map.empty

annotate' :: Context -> Core () -> Core CoreType
annotate' ctx (CIdent _ name)      = CIdent (typeOfIdent ctx name) name
annotate' ctx (CUnit _)            = CUnit CUnitTy
annotate' ctx (CInt _ value)       = CInt CIntTy value
annotate' ctx (CReal _ value)      = CReal CRealTy value
annotate' ctx (CText _ value)      = CText CTextTy value
annotate' ctx (CList _ values)     = CList CListTy $ map (annotate' ctx) values
annotate' ctx (CAssoc _ pairs)     = CAssoc CMapTy $ map (\(k, v) -> (annotate' ctx k, annotate' ctx v)) pairs
annotate' ctx (CMap _ pairs)       = CMap CMapTy $ Map.foldlWithKey' (\newMap k v -> Map.insert (annotate' ctx k) (annotate' ctx v) newMap) Map.empty pairs
annotate' ctx (CThunk _ env body)  = let annBody = annotate' ctx body
                                         in CThunk (CThunkTy $ meta annBody) env annBody
annotate' ctx (CFunc _ env param body) = let annBody = annotate' (withIdent param ctx) body
                                             in CFunc (CFuncTy (paramType param) (meta annBody)) env param annBody
annotate' ctx (CForce _ thunk)     = case annotate' ctx thunk of
                                       t@(CThunk (CThunkTy argTy) _ _) -> CForce argTy t
                                       t -> CForce CUnknownTy t
annotate' ctx (CApp _ func arg)    = let annArg = annotate' ctx arg
                                         in case annotate' ctx func of
                                              f@(CFunc (CFuncTy a b) _ _ _) -> if a == meta annArg
                                                                                 then CApp b f annArg
                                                                                 else CApp CUnknownTy f annArg
                                              f -> CApp CUnknownTy f annArg

paramType = snd
withIdent (name, ty) ctx = Map.insert name ty ctx
typeOfIdent ctx name = case Map.lookup name ctx of
                         Nothing -> CUnknownTy
                         Just a  -> a
