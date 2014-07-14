-- Idea is to traverse an AST while keeping track of where I came from.
import Control.Monad.State
import Data.Map as M

data MoleculeType = 
    TBool Bool
  | TInt Int
    deriving (Show, Eq)

data MoleculeExpr = 
    EVar String
  | ETrue | EFalse
  | EInt Int
  | ELet String MoleculeExpr MoleculeExpr
  | MoleculeExpr :+: MoleculeExpr 
  | MoleculeExpr :|: MoleculeExpr
    deriving (Show, Eq)

data MoleculeCrumb = 
    CPlusA MoleculeExpr        -- Came from (a +)
  | CPlusB MoleculeExpr        -- Came from (+ b)
  | COrA MoleculeExpr          -- Came from (a |)
  | COrB MoleculeExpr          -- Came from (| a)
  | CLetBe String MoleculeExpr -- Came from Let x be (expr) in ...
  | CLetIn String MoleculeExpr -- Came from Let x be ... in (expr)
    deriving (Show, Eq)

data MoleculeError = 
  TypeError String
  deriving (Show, Eq)

data MoleculeZipper = MoleculeZipper MoleculeExpr [MoleculeCrumb]
  deriving (Show, Eq)

testAST :: MoleculeExpr
testAST = ELet "a"  (EInt 10) (EVar "a" :+: EInt 10)