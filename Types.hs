-- Idea is to traverse an AST while keeping track of where I came from.
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader
import Data.Map as M

data MoleculeType = 
    TBool
  | TInt
  | TLam MoleculeType MoleculeType -- Function type
    deriving (Show, Eq)

type Name = String

data MoleculeValue = 
    VBool Bool
  | VInt Int
  | VLam ValueEnv TypeEnv Name MoleculeExpr

data MoleculeExpr = 
    EVar String
  | ETrue | EFalse
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
  deriving (Show, Eq)

type TypeEnv = M.Map Name MoleculeType
type ValueEnv = M.Map Name MoleculeValue

data MoleculeZipper = MoleculeZipper MoleculeExpr [MoleculeCrumb]
  deriving (Show, Eq)

testExpr :: MoleculeExpr
testExpr = EApp (EAbs "x" (EVar "x" :+: EVar "x")) (EInt 10)

type Typechecker = ExceptT MoleculeError (ReaderT TypeEnv (State (Maybe MoleculeCrumb)))

runTypecheck :: Maybe MoleculeCrumb -> TypeEnv -> MoleculeExpr -> Either MoleculeError MoleculeExpr
runTypecheck crumb te expr = evalState (runReaderT (runExceptT (typecheck expr)) te) crumb

typecheck :: MoleculeExpr -> Typechecker MoleculeExpr 
typecheck = undefined