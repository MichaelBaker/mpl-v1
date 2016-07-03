module ASTHelpers where

import Mpl.AST (AST(..))

aint    = AInt   ()
afloat  = AFloat ()
atext   = AText  ()
asym    = ASym   ()
aparen  = ASexp  () "(" ")"
asquare = ASexp  () "[" "]"
acurly  = ASexp  () "{" "}"
