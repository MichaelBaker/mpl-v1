module Mpl.Dyn.Desugar where

import Mpl.Dyn.Core (Core(..))
import Mpl.Dyn.AST  (AST(..))

desugar (ASym a span) = CSym a span
desugar a = undefined -- TODO: Make this total
