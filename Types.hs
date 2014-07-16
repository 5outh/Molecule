module Types where

import           Data.Map as M

data MoleculeType =
    TBool
  | TInt
  | TLam MoleculeType MoleculeType -- Function type
    deriving (Show, Eq)

type Name = String

data MoleculeValue =
    VBool Bool
  | VInt Int
  | VLam Env Name MoleculeExpr

data MoleculeExpr =
    EVar Name
  | ETrue 
  | EFalse
  | EInt Int
  | EAbs Name MoleculeExpr
  | EApp MoleculeExpr MoleculeExpr
  | MoleculeExpr :+: MoleculeExpr
  | MoleculeExpr :|: MoleculeExpr
    deriving (Show, Eq)

data MoleculeCrumb =
    CPlusA MoleculeExpr  -- Came from (a +)
  | CPlusB MoleculeExpr  -- Came from (+ b)
  | COrA MoleculeExpr    -- Came from (a |)
  | COrB MoleculeExpr    -- Came from (| a)
  | CAbs Name            -- Came from an abstraction
  | CApp1 MoleculeExpr   -- Came from first arg of application
  | CApp2 MoleculeExpr   -- Came from second arg of application
    deriving (Show, Eq)

data MoleculeError =
    TypeError String
  | ParseError String
  deriving (Show, Eq)

type Env = M.Map Name MoleculeValue
