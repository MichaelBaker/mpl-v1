module Mpl.TypeAnnotation where

import Data.Text (Text)
import Mpl.AST   (Core(..), CoreType(..), metaC, typeOf)

import Data.List  (foldl')
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as Map

type TypeMap a = Map.Map a CoreType
type Env       = Map.Map Text CoreType

annotate a = (a, check Map.empty Map.empty a)

check :: (Ord a, Show a, Show b) => Env -> TypeMap a -> Core a b -> TypeMap a
check env tm (CIdent i name)          = Map.insert i (typeOfIdent env name) tm
check env tm (CUnit i)                = Map.insert i CUnitTy tm
check env tm (CInt i _)               = Map.insert i CIntTy  tm
check env tm (CReal i _)              = Map.insert i CRealTy tm
check env tm (CText i _)              = Map.insert i CTextTy tm
check env tm (CList i values)         = Map.insert i CListTy   $ foldl' (\tm' -> check env tm') tm values
check env tm (CAssoc i pairs)         = Map.insert i CMapTy    $ foldl' (\tm' (k, v) -> check env (check env tm' v) k) tm pairs
check env tm (CMap i pairs)           = Map.insert i CMapTy    $ Map.foldlWithKey' (\tm' k v -> check env (check env tm' v) k) tm pairs

check env tm (CThunk i _ body) = let tm' = check env tm body
                                     in Map.insert i (CThunkTy $ fromJust $ Map.lookup (metaC body) tm') tm'

check env tm (CFunc i _ (param, tyParam) body) = let tm' = check (withIdent param tyParam env) tm body
                                                     in Map.insert i (CFuncTy tyParam (fromJust $ Map.lookup (metaC body) tm')) tm'

check env tm (CForce i thunk) = let tm' = check env tm thunk
                                    in case fromJust $ Map.lookup (metaC thunk) tm' of
                                         (CThunkTy t) -> Map.insert i t tm'
                                         _ -> Map.insert i CUnknownTy tm'

check env tm (CApp i func arg) = let tm'   = check env tm arg
                                     tm''  = check env tm' func
                                     argTy = fromJust $ Map.lookup (metaC arg)  tm''
                                     fTy   = fromJust $ Map.lookup (metaC func) tm''
                                     in case fTy of
                                          (CFuncTy a b) -> Map.insert i (if a == argTy then b else CUnknownTy) tm''
                                          _ -> Map.insert i CUnknownTy tm''

check _ _ a = error $ "Cannot annotate invalid term: " ++ show a

withIdent name ty env = Map.insert name ty env
typeOfIdent env name = case Map.lookup name env of
                         Nothing -> CUnknownTy
                         Just a  -> a


