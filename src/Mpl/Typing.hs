module Mpl.Typing where

import Mpl.Core  (Core(..), Type(..), Path, pathOf, metaOf, withMeta)
import Data.Text (Text)
import Control.Lens
import qualified Data.Set        as Set
import qualified Data.List       as List
import qualified Data.Map.Strict as Map

data TypeError = TypeError deriving (Show)

type Bindings = Set.Set Text

data Evidence =
    Literal Path Type
  | Body Path Type
  | ArgOf Path Type
  | FuncReturn Path Type
  deriving (Eq)

data CaseFile = CaseFile
  { _conclusion         :: Type
  , _evidence           :: [Evidence]
  , _identifierEvidence :: Map.Map Text [Evidence]
  , _conflicts          :: Set.Set Path
  }

makeLenses ''CaseFile

toTypedCore :: Core a -> (Maybe TypeError, Core Type)
toTypedCore core = undefined -- TODO

investigate :: Bindings -> Core a -> Core CaseFile

investigate _ (CInt path _ val) = CInt path (intLitCase path) val

investigate bindings (CIdent path _ name) =
  if isBound name bindings
    then CIdent path (boundIdentCase   path name) name
    else CIdent path (unboundIdentCase path name) name

investigate bindings (CThunk path _ body) =
  let bodyWithCase = investigate bindings body
      bodyCase     = metaOf bodyWithCase
      bodyType     = view conclusion bodyCase
      thunkCase    = bodyCase
                   & set conclusion (TThunk bodyType)
                   & set evidence [Body (pathOf bodyWithCase) bodyType]
      in CThunk path thunkCase bodyWithCase

investigate bindings (CApp path _ f arg) =
  let argWithCase = investigate bindings arg
      fWithCase   = investigate bindings f
      argType     = view conclusion (metaOf argWithCase)
      fType       = view conclusion (metaOf fWithCase)
      appCase     = case (fType, argType) of
                      (TFunc a b, TIdent c) -> caseFile & set conclusion b
                                                        & set identifierEvidence (Map.singleton c [ArgOf (pathOf f) a])
                      (TFunc a b, c) -> if a == c
                                          then caseFile & set conclusion b
                                                        & set evidence [FuncReturn (pathOf f) b]
                                          else caseFile & set conclusion TUnknown
                                                        & set conflicts (Set.singleton path)
                      _ -> caseFile
      in CApp path (List.foldl' merge appCase [metaOf argWithCase, metaOf fWithCase]) fWithCase argWithCase
      -- TODO: Polymorphic function

      -- newIdent    = CIdent iPath (set conclusion (TFunc TInt $ TFunc TInt TInt) caseFile) "+"
      -- newArg      = addEvidence (ArgOf iPath TInt) argWithCase
      -- argCase     = metaOf newArg
      -- appCase     = caseFile
      --             & set conclusion (TFunc TInt TInt)
      --             & set evidence [FuncReturn iPath (TFunc TInt TInt)]
      -- in CApp path (merge appCase argCase) newIdent newArg

investigate _ _ = undefined -- TODO

caseFile = CaseFile { _conclusion         = TUnknown
                    , _evidence           = []
                    , _conflicts          = emptyConflicts
                    , _identifierEvidence = emptyIdentifierEvidence
                    }

intLitCase path = caseFile
                & set conclusion TInt
                & set evidence [Literal path TInt]

unboundIdentCase path "+" = caseFile & set conclusion (TFunc TInt (TFunc TInt TInt))
unboundIdentCase path name = caseFile
                      & set conclusion TUnboundIdent
                      & set conflicts (Set.singleton path)

boundIdentCase path name = caseFile & set conclusion (TIdent name)

merge c1 c2 = c1
            & over conflicts (Set.union $ view conflicts c2)
            & over identifierEvidence (Map.unionWith (++) $ view identifierEvidence c2)

addEvidence e (CIdent path c name) =
  let newCase = c
              & over evidence (e :)
              & over identifierEvidence (Map.insertWith (++) name [e])
      in if hasConflict (view evidence newCase)
           then CIdent path (over conflicts (Set.insert path) newCase) name
           else CIdent path newCase name
addEvidence e c =
  let newCase = metaOf c & over evidence (e :)
      in if hasConflict (view evidence newCase)
           then withMeta c (over conflicts (Set.insert $ pathOf c) newCase)
           else withMeta c newCase

hasConflict []     = False
hasConflict (e:es) = fst $ List.foldl' conflicts (False, typeOf e) $ map typeOf es
  where typeOf (Literal _ ty)    = ty
        typeOf (Body _ ty)       = ty
        typeOf (ArgOf _ ty)      = ty
        typeOf (FuncReturn _ ty) = ty
        conflicts (alreadyConflicts, currentE) newE =
          if alreadyConflicts
            then (alreadyConflicts, currentE)
            else case (currentE, newE) of
                   (TPoly, b) -> (False, b)
                   (a, TPoly) -> (False, a)
                   (a, b)     -> if a == b
                                   then (False, a)
                                   else (True, a)

emptyIdentifierEvidence :: Map.Map Text [Evidence]
emptyIdentifierEvidence = Map.empty

emptyConflicts :: Set.Set Path
emptyConflicts = Set.empty

emptyBinding :: Bindings
emptyBinding = Set.empty

addBinding :: Text -> Bindings -> Bindings
addBinding = Set.insert

isBound :: Text -> Bindings -> Bool
isBound = Set.member

bindingsFromList :: [Text] -> Bindings
bindingsFromList = Set.fromList
