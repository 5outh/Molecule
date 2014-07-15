{-# LANGUAGE NoMonomorphismRestriction #-}
module Typechecker where

import           Types

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Identity
import           Data.Map             as M
import           Data.Set             as S
import           Lens.Family2
import           Lens.Family2.Stock

type TypeEnv     = M.Map Name MoleculeType
type Scope       = S.Set Name -- List of names in scope
type Typechecker = ExceptT MoleculeError (ReaderT (Maybe MoleculeCrumb, Scope) (StateT TypeEnv Identity))

runTypecheck :: Maybe MoleculeCrumb -> Scope -> TypeEnv -> MoleculeExpr -> Either MoleculeError MoleculeType
runTypecheck crumb scope te expr = runIdentity $ evalStateT (runReaderT (runExceptT (check expr)) (crumb, scope)) te

typecheck :: MoleculeExpr -> Either MoleculeError MoleculeType
typecheck = runTypecheck Nothing S.empty M.empty

addBinding :: Name -> MoleculeType -> Typechecker MoleculeType
addBinding name typ = modify (M.insert name typ) >> return typ

withCrumb :: Typechecker a -> MoleculeCrumb -> Typechecker a
withCrumb action crumb = local (_1 .~ Just crumb) action

withScopedVar :: Typechecker a -> Name -> Typechecker a
withScopedVar action var = local (_2 %~ S.insert var) action

withCrumbAndScopedVar :: Typechecker a -> MoleculeCrumb -> Name -> Typechecker a
withCrumbAndScopedVar action crumb var =
  local ((_1 .~ Just crumb) . (_2 %~ S.insert var)) action

check :: MoleculeExpr -> Typechecker MoleculeType
check (EVar name) = do
  (crumb, scope) <- ask
  if S.notMember name scope
  then throwError . TypeError $ "variable not in scope: " ++ name
  else do
    env <- get
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
          CApp1 _ -> throwError . TypeError $ name ++ " is not a function"
          CApp2 e -> do
            t <- check e
            case t of
              TLam v _ -> addBinding name v
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
          CAbs _ -> addBinding name t
          CApp1 _ -> case t of
            TLam _ _ -> addBinding name t
            _        -> throwError . TypeError $ name ++ " is not a function"
          CApp2 e -> do
            t' <- check e
            case t' of
              TLam typ _ | typ == t -> addBinding name t
                         | otherwise -> throwError . TypeError $ show e ++ " is not a function"
              _ -> throwError . TypeError $ show e ++ " is not a function"

check (e1 :+: e2) = do
  e1' <- check e1 `withCrumb` CPlusA e2
  e2' <- check e2 `withCrumb` CPlusB e1
  bindings <- get
  case (e1', e2') of
    (TInt, TInt) -> return TInt
    (_, TInt)    -> throwError . TypeError $ "type error in the first argument of +"
    (TInt, _)    -> throwError . TypeError $ "type error in the second argument of +"
    _            -> throwError . TypeError $ "type error in both arguments of +"

check (e1 :|: e2) = do
  e1' <- check e1 `withCrumb` COrA e2
  e2' <- check e2 `withCrumb` COrB e1
  case (e1', e2') of
    (TBool, TBool) -> return TBool
    (_, TBool)     -> throwError . TypeError $ "type error in the first argument of |"
    (TBool, _)     -> throwError . TypeError $ "type error in the second argument of |"
    _              -> throwError . TypeError $ "type error in both arguments of |"

check (EApp e1 e2) = do
  t' <- check e2 `withCrumb` CApp2 e1
  case e1 of
    EAbs name expr -> do
      addBinding name t'
      withCrumbAndScopedVar (check expr) (CAbs name) name
    _ -> do
      -- NB. e1 isn't a top-level lambda abstraction but it might typecheck to one.
      t <- check e1 `withCrumb` CApp1 e1
      case t of 
        TLam t1 t2 | t1 == t'  -> return t2
                   | otherwise -> throwError . TypeError $ "expecting " ++ show t1 ++ " but got " ++ show t' ++ " in function application"  
        _ -> throwError . TypeError $ "expecting a function, but got " ++ show e1 ++ " in function application"

check (EAbs name expr) = do
  t   <- withCrumbAndScopedVar (check expr) (CAbs name) name
  env <- get
  case M.lookup name env of
    Nothing -> throwError . TypeError $ "cannot infer type of " ++ name
    Just t' -> return $ TLam t' t

check ETrue    = return TBool
check EFalse   = return TBool
check (EInt _) = return TInt
