module Helper.AST where

import Mpl.Span (emptySpan)

import qualified Mpl.Dyn.AST as Dyn
import qualified Mpl.Ty.AST  as Ty

dyn_int     a   = Dyn.AInt     a   emptySpan
dyn_sym     a   = Dyn.ASym     a   emptySpan
dyn_real    a   = Dyn.AReal    a   emptySpan
dyn_text    a   = Dyn.AUtf16   a   emptySpan
dyn_prog    a   = Dyn.AProg    a   emptySpan
dyn_def     a b = Dyn.ADef     a b emptySpan
dyn_lam     a b = Dyn.ALam     a b emptySpan
dyn_recdefs a   = Dyn.ARecDefs a   emptySpan
dyn_let_exp a b = Dyn.ALet     a b emptySpan
dyn_rec     a   = Dyn.ARec     a   emptySpan
dyn_field   a b = Dyn.AField   a b emptySpan
dyn_list    a   = Dyn.AList    a   emptySpan
dyn_lens    a   = Dyn.ALens    a   emptySpan
dyn_app     a b = Dyn.AApp     a b emptySpan
dyn_lensapp a b = Dyn.ALensApp a b emptySpan

ty_int     a   = Ty.ADyn (dyn_int  a)
ty_sym     a   = Ty.ADyn (dyn_sym  a)
ty_real    a   = Ty.ADyn (dyn_real a)
ty_utf16   a   = Ty.ADyn (dyn_text a)
ty_ty      a   = Ty.ATySym   a   emptySpan
ty_lens    a   = Ty.ALens    a   emptySpan
ty_list    a   = Ty.AList    a   emptySpan
ty_rec     a   = Ty.ARec     a   emptySpan
ty_ann_exp a b = Ty.AAnnExp  a b emptySpan
ty_field   a b = Ty.AField   a b emptySpan
ty_let_exp a b = Ty.ALet     a b emptySpan
ty_def     a b = Ty.ADef     a b emptySpan
ty_lam     a b = Ty.ALam     a b emptySpan
ty_app     a b = Ty.AApp     a b emptySpan
ty_lensapp a b = Ty.ALensApp a b emptySpan
