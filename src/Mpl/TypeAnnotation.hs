module Mpl.TypeAnnotation where

import Data.Text (Text)
import Mpl.AST   (Core(..), CoreType(..), meta)

import Data.List  (foldl')
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as Map

type Context = Map.Map Text  CoreType
type TypeMap = Map.Map [Int] CoreType

annotate a = (a, annotate' Map.empty Map.empty a)

annotate' :: Context -> TypeMap -> Core [Int] -> TypeMap
annotate' ctx tm (CIdent path name)    = Map.insert path (typeOfIdent ctx name) tm
annotate' ctx tm (CUnit path)          = Map.insert path CUnitTy tm
annotate' ctx tm (CInt path _)     = Map.insert path CIntTy tm
annotate' ctx tm (CReal path _)    = Map.insert path CRealTy tm
annotate' ctx tm (CText path _)    = Map.insert path CTextTy tm
annotate' ctx tm (CList path values)   = Map.insert path CListTy $ foldl' (\tm' -> annotate' ctx tm') tm values
annotate' ctx tm (CAssoc path pairs)     = Map.insert path CMapTy $ foldl' (\tm' (k, v) -> annotate' ctx (annotate' ctx tm' v) k) tm pairs

annotate' ctx tm (CMap path pairs)       = Map.insert path CMapTy $ Map.foldlWithKey' (\tm' k v -> annotate' ctx (annotate' ctx tm' v) k) tm pairs
annotate' ctx tm (CThunk path env body)  = let tm' = annotate' ctx tm body
                                            in Map.insert path (CThunkTy $ fromJust $ Map.lookup (meta body) tm') tm'
annotate' ctx tm (CFunc path env param body) = let tm' = annotate' (withIdent param ctx) tm body
                                                in Map.insert path (CFuncTy (paramType param) (fromJust $ Map.lookup (meta body) tm')) tm'
annotate' ctx tm (CForce path thunk) = let tm' = annotate' ctx tm thunk
                                           in case fromJust $ Map.lookup (meta thunk) tm' of
                                            (CThunkTy t) -> Map.insert path t tm'
                                            _ -> Map.insert path CUnknownTy tm'
annotate' ctx tm (CApp path func arg) = let tm'   = annotate' ctx tm arg
                                            tm''  = annotate' ctx tm' func
                                            argTy = fromJust $ Map.lookup (meta arg)  tm''
                                            fTy   = fromJust $ Map.lookup (meta func) tm''
                                            in case fTy of
                                              (CFuncTy a b) -> Map.insert path (if a == argTy then b else CUnknownTy) tm''
                                              _ -> Map.insert path CUnknownTy tm''

paramType = snd
withIdent (name, ty) ctx = Map.insert name ty ctx
typeOfIdent ctx name = case Map.lookup name ctx of
                         Nothing -> CUnknownTy
                         Just a  -> a
