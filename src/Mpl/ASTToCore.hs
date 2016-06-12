module Mpl.ASTToCore where

import Mpl.AST  (AST(..))
import Mpl.Core (Core(..), Term)

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
elaborate _ = undefined

typeAnnotation (a:b:[]) = CTyAnn (ty a) (elaborate b)
typeAnnotation a = error $ "Invalid type annotation: " ++ show a

ty (ASexp "(" ")" [ASym "->", ASym a, ASym b]) = CLamTy (CTyParam a) (CTyParam b)
ty (ASym "int") = CIntTy
ty (ASym a)     = CTyParam a
ty a = error $ "Invalid type: " ++ show a

application _ (ASexp "(" ")" ((ASym "@"):rest)) arg = CPolyApp (polyFunc rest) (ty arg)
application _ (ASexp "(" ")" ((ASym "#"):rest)) arg = CTermApp (lambda rest) (elaborate arg)
application sexp _ _ = error $ "Invalid application: " ++ show sexp

lambda [ASexp "[" "]" [ASexp "(" ")" [ASym ":", ASym param, tyParam]], body] = CLam param (ty tyParam)$ elaborate body
lambda a = error $ "Invalid lambda: " ++ show a

polyFunc [ASexp "[" "]" [ASym param], body] = CPolyFunc param $ elaborate body
polyFunc a = error $ "Invalid polymorphic function: " ++ show a
