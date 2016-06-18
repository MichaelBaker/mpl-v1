module ASTHelpers where

import Mpl.AST (AST(..))

aint    = AInt   ()
asym    = ASym   ()
aparen  = ASexp  () "(" ")"
asquare = ASexp  () "[" "]"
