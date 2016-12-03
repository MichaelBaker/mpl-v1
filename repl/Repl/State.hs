module Repl.State where

data State =
  State
    { language         :: Language
    , mode             :: Mode
    , defaultTermWidth :: Int
    } deriving (Show, Read)

data Language =
  Untyped
  deriving (Show, Read, Enum)

data Mode =
    AST
  | Echo
  | LLVM
  | JS
  deriving (Show, Read, Enum)

data StateLineItem =
  StateLineItem
    { lineString   :: String
    , lineMaxWidth :: Int
    }

showStateLine extraPadding a = lineString a ++ replicate padding ' '
  where totalPadding = extraPadding + lineMaxWidth a
        padding      = totalPadding - length (lineString a)

toStateLineItem :: (Enum a, Show a) => Maybe String -> a -> StateLineItem
toStateLineItem label a = StateLineItem { lineString = string, lineMaxWidth = totalMaxWidth }
  where string        = labelString ++ show a
        totalMaxWidth = maxWidth a + length labelString
        labelString   =
          case label of
            Nothing -> ""
            Just l -> l ++ ": "

strLineItem string maxWidth = StateLineItem { lineString = string, lineMaxWidth = maxWidth }

maxWidth :: (Enum a, Show a) => a -> Int
maxWidth = maximum . map (length . show) . enumFrom

defaultState =
  State
    { language         = Untyped
    , mode             = AST
    , defaultTermWidth = 80
    }
