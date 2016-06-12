module Mpl.ASTToCore where

import Mpl.AST  (AST(..))
import Mpl.Core (Core(..), Term)
import qualified Data.Map.Strict as Map

astToCore :: AST -> Core Term
astToCore ast = elaborate ast

elaborate (AInt a)   = CInt a
elaborate (AFloat a) = CReal a
elaborate (AText a)  = CText a
elaborate (ASym a)   = CSym a
elaborate a@(ASexp "(" ")" ((ASym "#"):rest)) = lambda rest
elaborate a@(ASexp "(" ")" ((ASym ":"):rest)) = typeAnnotation rest
elaborate a@(ASexp "(" ")" ((ASym "@"):_))    = error $ "Unapplied polymorphic function: " ++ show a
elaborate a@(ASexp "(" ")" [f, arg])          = application a f arg
elaborate a@(ATagSexp "#" "{" "}" items)      = record a items
elaborate _ = undefined

record sexp items = CRecord $ Map.fromList $ makeItems items
  where makeItems []                  = []
        makeItems (_:[])              = error $ "Odd number of items in record: " ++ show sexp
        makeItems ((ASym key):v:rest) = (key, elaborate v) : makeItems rest
        makeItems _                   = error $ "Invalid record field: " ++ show sexp

typeAnnotation (a:b:[]) = CTyAnn (ty a) (elaborate b)
typeAnnotation a = error $ "Invalid type annotation: " ++ show a

ty (ASym "int") = CIntTy
ty (ASym a)     = CTyParam a
ty (ASexp "(" ")" [ASym sym, a, b]) = CTyPrim sym (ty a) (ty b)
ty (ASexp "(" ")" (f:arg:[])) = CTyLamApp (tyLam f) (ty arg)
ty a@(ATagSexp "#" "{" "}" items) = recordTy a items
ty a = error $ "Invalid type: " ++ show a

recordTy sexp items = CRecordTy $ Map.fromList $ makeItems items
  where makeItems []                  = []
        makeItems (_:[])              = error $ "Odd number of items in record type: " ++ show sexp
        makeItems ((ASym key):v:rest) = (key, ty v) : makeItems rest
        makeItems _                   = error $ "Invalid record type field: " ++ show sexp

application _ (ASexp "(" ")" ((ASym "@"):rest)) arg = CPolyApp (polyFunc rest) (ty arg)
application _ (ASexp "(" ")" ((ASym "#"):rest)) arg = CTermApp (lambda rest) (elaborate arg)
application sexp _ _ = error $ "Invalid application: " ++ show sexp

lambda [ASexp "[" "]" [ASexp "(" ")" [ASym ":", ASym param, tyParam]], body] = CLam param (ty tyParam)$ elaborate body
lambda a = error $ "Invalid lambda: " ++ show a

polyFunc [ASexp "[" "]" [ASym param], body] = CPolyFunc param $ elaborate body
polyFunc a = error $ "Invalid polymorphic function: " ++ show a

tyLam (ASexp "(" ")" [ASym "$", (ASexp "[" "]" [ASym param]), body]) = CTyLam param $ ty body
tyLam a = error $ "Invalid type operator: " ++ show a
