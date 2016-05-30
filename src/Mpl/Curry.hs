module Mpl.Curry where

import Mpl.AST (AST(..))

curry :: AST () -> AST ()
curry (AFunc _ as body) = loop as body
curry a = a

loop [] body     = AFunc () [] body
loop (a:[]) body = AFunc () [a] body
loop (a:as) body = AFunc () [a] (loop as body)
