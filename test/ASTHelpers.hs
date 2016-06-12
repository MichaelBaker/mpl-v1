module ASTHelpers where

import Mpl.AST (AST(..))

aint    = AInt
afloat  = AFloat
atext   = AText
asym    = ASym
aparen  = ASexp "(" ")"
asquare = ASexp "[" "]"
acurly  = ASexp "{" "}"

app a b = aparen [a, b]

alam var ty body = aparen [asym "#",
  asquare [aparen [asym ":", asym var, asym ty]],
  body]

poly ty term = aparen [asym "@",
  asquare [asym ty],
  term]

tyan ty term = aparen [asym ":", ty, term]
