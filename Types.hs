module Types where

import           Data.Map as M

data MoleculeType =
    TBool
  | TInt
  | TLam MoleculeType MoleculeType -- Function type
    deriving (Eq)

showType :: MoleculeType -> String
showType TBool = "Bool"
showType TInt  = "Int"
showType (TLam t1 t2) = showType t1 ++ " -> " ++ showType t2

instance Show MoleculeType where
  show = showType

type Name = String

data MoleculeValue =
    VBool Bool
  | VInt Int
  | VLam Env Name MoleculeExpr

showValue :: MoleculeValue -> String
showValue (VBool b) = case b of
  True  -> "t"
  False -> "f"
showValue (VInt n) = show n
showValue (VLam _ nm e) = "\\" ++ nm ++ " . " ++ show e

instance Show MoleculeValue where
  show = showValue

data MoleculeExpr =
    EVar Name
  | ETrue 
  | EFalse
  | EInt Int
  | EAbs Name MoleculeExpr
  | EApp MoleculeExpr MoleculeExpr
  | MoleculeExpr :+: MoleculeExpr
  | MoleculeExpr :|: MoleculeExpr
    deriving Eq

-- naïve @TODO: Make better
showExpr :: MoleculeExpr -> String
showExpr (EVar nm) = nm
showExpr ETrue     = "t"
showExpr EFalse    = "f"
showExpr (EInt n)  = show n
showExpr (a :+: b) = show a ++ " + " ++ show b
showExpr (a :|: b) = show a ++ " | " ++ show b
showExpr (EAbs nm e) = "\\" ++ nm ++ " . " ++ show e
-- NB. this in particular is ugly
showExpr (EApp e1 e2) = "(" ++ show e1 ++ ") (" ++ show e2 ++ ")"

instance Show MoleculeExpr where
  show = showExpr

data MoleculeCrumb =
    CPlus MoleculeExpr  -- Came from (+ b)
  | COr MoleculeExpr    -- Came from (| a)
  | CAbs Name            -- Came from an abstraction
  | CApp1 MoleculeExpr   -- Came from first arg of application
  | CApp2 MoleculeExpr   -- Came from second arg of application
    deriving (Show, Eq)

data MoleculeError =
    TypeError String
  | ParseError String
  | RuntimeError String
    deriving (Show, Eq)

type Env = M.Map Name MoleculeValue
