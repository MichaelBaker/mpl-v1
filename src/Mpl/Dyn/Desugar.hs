module Mpl.Dyn.Desugar where

import Mpl.Dyn.Core (Core(..), CoreLabel(..), CoreBind(..))
import Mpl.Dyn.AST  (AST(..))

desugar (ASym     a span)     = CSym   a span
desugar (AInt     a span)     = CInt   a span
desugar (AReal    a span)     = CReal  a span
desugar (AUtf16   a span)     = CUtf16 a span
desugar (AList    a span)     = CList  (map desugar a) span
desugar (ARec     a span)     = CRec   (map fromField a) span
desugar (ALet     a  b span)  = CLet   (map fromBind a) (desugar b) span
desugar (ALam     [] b span)  = CThunk (desugar b) span
desugar (ALam     a  b span)  = makeLambda (map fromSym a) (desugar b) span
desugar (ALens    a    span)  = CLens (map desugar a) span
desugar (AApp     a  b span)  = makeApplication (desugar a) (map desugar b) span
desugar (ALensApp a b span)   = CApp (desugar a) (desugar b) span
desugar a = undefined -- TODO: Make this total

makeLambda []     body span = body
makeLambda (a:as) body span = CLam a (makeLambda as body span) span

makeApplication f [] _        = f
makeApplication f (a:as) span = makeApplication (CApp f a span) as span

fromField (AField (ASym a _) b _) = (CLSym a, desugar b)
fromField (AField (AInt a _) b _) = (CLInt a, desugar b)
fromField a = error $ "Invalid record field: " ++ show a -- TODO fold this into error reporting

fromBind (ADef (ASym name _) value _) = (CoreBind name, desugar value)
fromBind a = error $ "Invalid binding: " ++ show a -- TODO fold this into error reporting

fromSym (ASym name _) = CoreBind name
fromSym a = error $ "Invalid symbol: " ++ show a -- TODO fold this into error reporting
