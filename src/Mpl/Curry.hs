module Mpl.Curry where

import Mpl.AST (AST(..))
import Prelude hiding (curry)

curry :: AST () -> AST ()
curry f@(ACFunc _ _ _)  = f
curry a@(ACApp _ _ _)   = a
curry (AFunc _ as body) = recurFunc as body
curry (AApp  _ f as)    = recurApp (curry f) as
curry a = a

recurFunc [] body     = ACFunc () Nothing  (curry body)
recurFunc (a:[]) body = ACFunc () (Just a) (curry body)
recurFunc (a:as) body = ACFunc () (Just a) (recurFunc as body)

recurApp a@(ACApp _ _ _) [] = a
recurApp app []             = ACApp () app Nothing
recurApp app (a:as)         = recurApp (ACApp () app (Just a)) as
