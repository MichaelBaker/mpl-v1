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

toTypedCore :: Core a -> (Maybe TypeError, Core CaseFile)
toTypedCore core =
  let coreWithCase  = investigate emptyBinding core
      coreConflicts = view conflicts (metaOf coreWithCase)
      in if Set.null coreConflicts
           then (Nothing, coreWithCase)
           else (Just TypeError, coreWithCase)

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

investigate bindings (CFunc path _ param body) =
  let bodyWithCase  = investigate (addBinding param bindings) body
      bodyType      = view conclusion (metaOf bodyWithCase)
      identEvidence = view identifierEvidence (metaOf bodyWithCase)
      paramType     = typeOfParam identEvidence param
      funcCase      = caseFile & set conclusion (TFunc paramType bodyType)
      in CFunc path (merge funcCase $ metaOf bodyWithCase) param bodyWithCase

investigate bindings (CApp path _ f arg) =
  let argWithCase = investigate bindings arg
      fWithCase   = investigate bindings f
      argType     = view conclusion (metaOf argWithCase)
      fType       = view conclusion (metaOf fWithCase)
      appCase     = case (fType, argType) of
                      (TFunc TPoly b, _) -> caseFile & set conclusion b
                      (TFunc a b, TIdent c) -> caseFile & set conclusion b
                                                        & set identifierEvidence (Map.singleton c [ArgOf (pathOf f) a])
                      (TFunc a b, c) -> if a == c
                                          then caseFile & set conclusion b
                                                        & set evidence [FuncReturn (pathOf f) b]
                                          else caseFile & set conclusion TUnknown
                                                        & set conflicts (Set.singleton path)
                      _ -> caseFile & set conflicts (Set.singleton path)
      in CApp path (List.foldl' merge appCase [metaOf argWithCase, metaOf fWithCase]) fWithCase argWithCase

investigate bindings (CForce path _ f) =
  let fWithCase = investigate bindings f
      fCase     = metaOf fWithCase
      forceCase = case view conclusion fCase of
                    (TThunk a) -> caseFile & set conclusion a
                    _          -> caseFile & set conflicts (Set.singleton path)
      in CForce path (merge forceCase fCase) fWithCase

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

typeOfParam ev p = case Map.lookup p ev of
                     Nothing -> TPoly
                     Just e  -> snd $ formConclusion e

typeOf (Literal _ ty)    = ty
typeOf (Body _ ty)       = ty
typeOf (ArgOf _ ty)      = ty
typeOf (FuncReturn _ ty) = ty

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

hasConflict = fst . formConclusion

formConclusion []  = (False, TUnknown)
formConclusion (e:es) = List.foldl' conflicts (False, typeOf e) $ map typeOf es
  where conflicts (alreadyConflicts, currentE) newE =
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