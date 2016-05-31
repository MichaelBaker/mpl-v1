module Mpl.TypeAnnotation where

import Mpl.AST (Core(..))

import Mpl.AST (Core(..), CoreType(..), meta)

annotate (CUnit _) = CUnit CUnitTy
annotate (CInt _ value) = CInt CIntTy value
annotate (CReal _ value) = CReal CRealTy value
annotate (CText _ value) = CText CTextTy value
annotate (CThunk _ body) = let annBody = annotate body in CThunk (CThunkTy $ meta annBody) annBody
annotate _ = undefined -- TODO: Remove this and align everything
