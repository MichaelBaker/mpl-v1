module Mpl.TypeAnnotation where

import Data.Text (Text)
import Mpl.AST   (Core(..), CoreType(..), Context, emptyContext, meta)

import Data.List  (foldl')
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as Map

type TypeMap = Map.Map [Int] CoreType

annotate a = (a, check emptyContext Map.empty a)

check :: Context -> TypeMap -> Core [Int] -> TypeMap
check ctx tm (CIdent path name)  = Map.insert path (typeOfIdent ctx name) tm
check ctx tm (CUnit path)        = Map.insert path CUnitTy tm
check ctx tm (CInt path _)       = Map.insert path CIntTy tm
check ctx tm (CReal path _)      = Map.insert path CRealTy tm
check ctx tm (CText path _)      = Map.insert path CTextTy tm
check ctx tm (CList path values) = Map.insert path CListTy $ foldl' (\tm' -> check ctx tm') tm values
check ctx tm (CAssoc path pairs) = Map.insert path CMapTy $ foldl' (\tm' (k, v) -> check ctx (check ctx tm' v) k) tm pairs
check ctx tm (CMap path pairs)   = Map.insert path CMapTy $ Map.foldlWithKey' (\tm' k v -> check ctx (check ctx tm' v) k) tm pairs
check ctx tm (CThunk path env _ body) = let tm' = check ctx tm body
                                            in Map.insert path (CThunkTy $ fromJust $ Map.lookup (meta body) tm') tm'
check ctx tm (CFunc path env _ (param, tyParam) body) = let tm' = check (withIdent param ctx) tm body
                                                            in Map.insert path (CFuncTy (typeOfIdent ctx tyParam) (fromJust $ Map.lookup (meta body) tm')) tm'
check ctx tm (CTyFunc path env _ param body) = Map.insert path CUnknownTy $ check ctx tm body
check ctx tm (CForce path thunk) = let tm' = check ctx tm thunk
                                       in case fromJust $ Map.lookup (meta thunk) tm' of
                                            (CThunkTy t) -> Map.insert path t tm'
                                            _ -> Map.insert path CUnknownTy tm'
check ctx tm (CApp path func arg) = let tm'   = check ctx tm arg
                                        tm''  = check ctx tm' func
                                        argTy = fromJust $ Map.lookup (meta arg)  tm''
                                        fTy   = fromJust $ Map.lookup (meta func) tm''
                                        in case fTy of
                                             (CFuncTy a b) -> Map.insert path (if a == argTy then b else CUnknownTy) tm''
                                             _ -> Map.insert path CUnknownTy tm''

withIdent name ctx = Map.insert name CUnknownTy ctx
typeOfIdent ctx name = case Map.lookup name ctx of
                         Nothing -> CUnknownTy
                         Just a  -> a
