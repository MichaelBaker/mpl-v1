module Mpl.ASTToCore where

import Mpl.AST  (AST(..))
import Mpl.Core (Core(..), Type(..))

toCore = elaborate [0]

elaborate :: [Int] -> AST -> Either String Core
elaborate path (AInt value) = Right $ CInt path TInt value
elaborate path (ASym value) = Right $ CIdent path TUnknown value
elaborate path (ASexp "(" ")" [ASym "#", ASexp "[" "]" params, body]) = elaborateFunc path params body
elaborate path (ASexp "(" ")" (f:args)) = curryApplication path f $ reverse args
elaborate _ a = Left $ "Invalid s-expression: " ++ show a

elaborateFunc path [] body = do
  body' <- elaborate (0:path) body
  return $ CThunk path TUnknown body'
elaborateFunc path (sym:[]) body = do
  body' <- elaborate (0:path) body
  sym'  <- symName sym
  return $ CFunc path TUnknown sym' body'
elaborateFunc path (sym:rest) body = do
  body' <- elaborateFunc (0:path) rest body
  sym'  <- symName sym
  return $ CFunc path TUnknown sym' body'

curryApplication path f [] = do
  thunk' <- elaborate (0:path) f
  return $ CForce path TUnknown thunk'
curryApplication path f (a:[]) = do
  func' <- elaborate (0:path) f
  arg'  <- elaborate (1:path) a
  return $ CApp path TUnknown func' arg'
curryApplication path f (a:as) = do
  func' <- curryApplication (0:path) f as
  arg'  <- elaborate (1:path) a
  return $ CApp path TUnknown func' arg'

symName (ASym name) = Right name
symName a           = Left $ "Invalid parameter name: " ++ show a
