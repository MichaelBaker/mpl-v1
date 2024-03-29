module Mpl.AnnotationSpec where

import TestUtils (describe, it, shouldBe)

import GHC.Generics (Generic)
import Mpl.Annotation
import Data.Functor.Foldable (Fix(..), cata)

data Flat r = A | B r r deriving (Show, Functor, Generic, Eq, Foldable, Traversable)

data Nest r = N (Flat r) deriving (Show, Functor, Generic, Eq, Foldable, Traversable)

n = Fix . N

spec = do
  let simpleAst = n A
  let ast       = n $ B (n A) (n A)
  let bigAst    = n $ B (n $ B (n A) (n A)) (n $ B (n A) (n A))
  let numbered  = annotateWithState (0 :: Int) (+ 1)

  describe "annotateWithState" $ do
    it "annotates a simple AST statefully from top to bottom" $ do
      numbered simpleAst `shouldBe` (0 :< N A)

    it "annotates a nested AST statefully from top to bottom" $ do
      numbered ast `shouldBe`
        (0 :< N
          (B
            (1 :< N A)
            (2 :< N A)))

    it "annotates a deeply nested AST statefully from top to bottom" $ do
      numbered bigAst `shouldBe`
        (0 :< N
          (B
            (1 :< N
              (B
                (2 :< N A)
                (3 :< N A)))
            (4 :< N
              (B
                (5 :< N A)
                (6 :< N A)))))

    it "is Foldable over the annotated datatype" $ do
      let countNest (N f)     = 1 + countFlat f
          countFlat A         = 1
          countFlat (B c1 c2) = c1 + c2 + 1

      cata countNest simpleAst `shouldBe` 2
      cata countNest ast       `shouldBe` 6
      cata countNest bigAst    `shouldBe` 14

      cata countNest (numbered simpleAst) `shouldBe` cata countNest simpleAst
      cata countNest (numbered ast)       `shouldBe` cata countNest ast
      cata countNest (numbered bigAst)    `shouldBe` cata countNest bigAst

  describe "envcata" $ do
    it "makes the annotation available at each step of recursion" $ do
      let nameNest i (N f)     = nameFlat i f
          nameFlat i A         = show i
          nameFlat i (B c1 c2) = "(" ++ show i ++ " " ++ c1 ++ " " ++ c2 ++ ")"

      envcata nameNest (numbered simpleAst) `shouldBe` "0"
      envcata nameNest (numbered ast)       `shouldBe` "(0 1 2)"
      envcata nameNest (numbered bigAst)    `shouldBe` "(0 (1 2 3) (4 5 6))"
