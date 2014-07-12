{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

import Data.Foldable
import Data.Traversable

data MoleculeType = 
    TBool Bool
  | TInt Int
    deriving (Show, Eq)

data MoleculeExpr e = 
    EVar String
  | ETrue | EFalse
  | EInt Int
  | EIf e e e
  | ELet String e e
  | e :+: e
    deriving (Show, Eq, Functor, Foldable, Traversable)

data ASTCrumb e = 
    CIfPred e e -- Came from predicate
  | CIfThen e e -- Came from "then"
  | CIfElse e e -- Came from "else"
  | CPlusA e               -- Came from (a +)
  | CPlusB e               -- Came from (+ b)
  | CLetBe String e        -- Came from Let x be (expr) in ...
  | CLetIn String e        -- Came from Let x be ... in (expr)
    deriving (Show, Eq, Functor, Foldable, Traversable)

data ASTZipper e = ASTZipper e [ASTCrumb e]
  deriving (Show, Eq, Functor, Foldable, Traversable)
