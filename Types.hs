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
    EVar Name
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
testExpr2 = EAbs "x" (EVar "x" :+: EVar "x")
failExpr = (ETrue :+: (EInt 10))

type Typechecker = ExceptT MoleculeError (ReaderT (Maybe MoleculeCrumb) (State TypeEnv))

runTypecheck :: Maybe MoleculeCrumb -> TypeEnv -> MoleculeExpr -> Either MoleculeError MoleculeType
runTypecheck crumb te expr = evalState (runReaderT (runExceptT (typeof expr)) crumb) te

addBinding :: Name -> MoleculeType -> Typechecker MoleculeType
addBinding name typ = modify (M.insert name typ) >> return typ

typeof :: MoleculeExpr -> Typechecker MoleculeType
typeof (EVar name) = do
  crumb <- ask
  env   <- get
  case M.lookup name env of
    Nothing -> case crumb of
      Nothing -> throwError . TypeError $ "cannot unify type for top-level variable " ++ name
      Just x -> case x of
        CPlusA _ -> addBinding name TInt
        CPlusB _ -> addBinding name TInt
        COrA   _ -> addBinding name TBool
        COrB   _ -> addBinding name TBool
        CAbs nm -> case M.lookup nm env of 
          Just t  -> addBinding name t 
          Nothing -> throwError . TypeError $ "cannot unify type for variable " ++ nm
        CApp1 e -> throwError . TypeError $ name ++ " is not a function"
        CApp2 e -> do
          t <- typeof e
          case t of 
            TLam v e -> addBinding name v
            _ -> throwError . TypeError $ "cannot apply non-function to " ++ name
    Just t -> case crumb of
      Nothing -> addBinding name t
      Just cb -> case cb of 
        CPlusA _ | t == TInt -> addBinding name TInt
                 | otherwise -> throwError . TypeError $ "type error in the first argument of +"
        CPlusB _ | t == TInt -> addBinding name TInt
                 | otherwise -> throwError . TypeError $ "type error in the second argument of +"
        COrA   _ | t == TBool -> addBinding name TBool
                 | otherwise -> throwError . TypeError $ "type error in the first argument of |"
        COrB   _ | t == TBool -> addBinding name TBool
                 | otherwise -> throwError . TypeError $ "type error in the second argument of |"
        CAbs nm -> addBinding name t
        CApp1 e -> case t of
          TLam _ _ -> addBinding name t 
          _        -> throwError . TypeError $ name ++ " is not a function"
        CApp2 e -> do
          t' <- typeof e
          case t of
            TLam typ _ | typ == t -> addBinding name t
                       | otherwise -> throwError . TypeError $ show e ++ " is not a function"
            _ -> throwError . TypeError $ show e ++ " is not a function" 
typeof ETrue    = return TBool
typeof EFalse   = return TBool
typeof (EInt _) = return TInt

typeof (e1 :+: e2) = do
  e1' <- local (const . Just $ CPlusA e2) (typeof e1)
  e2' <- local (const . Just $ CPlusB e1) (typeof e2)
  case (e1', e2') of
    (TInt, TInt) -> return TInt
    (t, TInt)    -> throwError . TypeError $ "type error in the first argument of +"
    (TInt, t)    -> throwError . TypeError $ "type error in the second argument of +"
    _            -> throwError . TypeError $ "type error in both arguments of +"

typeof (e1 :|: e2) = do
  e1' <- local (const . Just $ COrA e2) (typeof e1)
  e2' <- local (const . Just $ COrB e1) (typeof e2)
  case (e2', e2') of
    (TBool, TBool) -> return TBool
    (t, TBool)     -> throwError . TypeError $ "type error in the first argument of |"
    (TBool, t)     -> throwError . TypeError $ "type error in the second argument of |"
    _              -> throwError . TypeError $ "type error in both arguments of |"

typeof (EApp e1 e2) = do
  t  <- local (const . Just $ CApp1 e2) (typeof e1)
  t' <- local (const . Just $ CApp2 e1) (typeof e2)
  case t of
    TLam t1 t2 | t1 == t'  -> return t1
               | otherwise -> throwError . TypeError $ "type error in function application"
    _ -> throwError . TypeError $ "type error in function application"  

typeof (EAbs name expr) = do
  t   <- local (const . Just $ CAbs name) (typeof expr)
  env <- get
  case M.lookup name env of
    Nothing -> throwError . TypeError $ "cannot infer type of " ++ name
    Just t' -> return $ TLam t' t  

test = runTypecheck Nothing M.empty