module Mpl.Typing where

import Mpl.Core  (Core(..), Type(..), Path, pathOf, metaOf)
import Data.Text (Text)
import Control.Lens
import qualified Data.Set        as Set
import qualified Data.Map.Strict as Map

data TypeError = TypeError deriving (Show)

type Bindings = Set.Set Text

data Evidence =
    Literal Path Type
  | Body Path Type
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
    then CIdent path (boundIdentCase path)   name
    else CIdent path (unboundIdentCase path) name

investigate bindings (CThunk path _ body) =
  let bodyWithCase = investigate bindings body
      bodyCase     = metaOf bodyWithCase
      bodyType     = view conclusion bodyCase
      thunkCase    = bodyCase
                   & set conclusion (TThunk bodyType)
                   & set evidence [Body (pathOf bodyWithCase) bodyType]
      in CThunk path thunkCase bodyWithCase

investigate _ _ = undefined -- TODO

caseFile = CaseFile { _conclusion         = TUnknown
                    , _evidence           = []
                    , _conflicts          = emptyConflicts
                    , _identifierEvidence = emptyIdentifierEvidence
                    }

intLitCase path = caseFile
                & set conclusion TInt
                & set evidence [Literal path TInt]

unboundIdentCase path = caseFile
                      & set conclusion TUnboundIdent
                      & set conflicts (Set.singleton path)

boundIdentCase path = caseFile & set conclusion TPoly

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
