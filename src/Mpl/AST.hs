module Mpl.AST where

import Data.Text (Text)
import qualified Data.Map as Map

class Meta a where
  meta :: a b -> b

type Env = Map.Map Text (Core [Int])
emptyEnv = Map.empty :: Env

data Core a = CInt   a Integer
            | CIdent a Text
            | CThunk a Env (Core a)
            | CForce a (Core a)
            | CFunc  a Env Text (Core a)
            | CApp   a (Core a) (Core a)
            deriving (Show, Eq, Ord)

instance Meta Core where
  meta (CInt    a _)     = a
  meta (CIdent  a _)     = a
  meta (CThunk  a _ _)   = a
  meta (CForce  a _)     = a
  meta (CFunc   a _ _ _) = a
  meta (CApp    a _ _)   = a

data AST a = AInt   a Integer
           | ASym   a Text
           | ASexp  a Text Text [AST a]
           deriving (Show, Eq)

instance Meta AST where
  meta (AInt   a _)     = a
  meta (ASym a _)       = a
  meta (ASexp  a _ _ _) = a
