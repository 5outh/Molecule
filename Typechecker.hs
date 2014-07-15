{-# LANGUAGE NoMonomorphismRestriction #-}
module Typechecker where

import Types

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader
import Lens.Family2
import Lens.Family2.Stock
import Data.Map as M
import Data.Set as S

testExpr :: MoleculeExpr
testExpr  = EApp (EAbs "x" (EVar "x" :+: EVar "x")) (EInt 10)
testExpr2 = EAbs "x" (EVar "x" :+: EVar "x")
testExpr3 = EAbs "a" (EAbs "b" (EVar "a" :|: EVar "b"))
failExpr  = ETrue :+: (EInt 10)
failExpr2 = EAbs "a" (EVar "a" :|: EVar "b") -- succeeds with Bool -> Bool due to lack of scope

type TypeEnv     = M.Map Name MoleculeType
type Scope       = S.Set Name -- List of names in scope
type Typechecker = ExceptT MoleculeError (ReaderT (Maybe MoleculeCrumb, Scope) (State TypeEnv))

runTypecheck :: Maybe MoleculeCrumb -> Scope -> TypeEnv -> MoleculeExpr -> Either MoleculeError MoleculeType
runTypecheck crumb scope te expr = evalState (runReaderT (runExceptT (typecheck expr)) (crumb, scope)) te

addBinding :: Name -> MoleculeType -> Typechecker MoleculeType
addBinding name typ = modify (M.insert name typ) >> return typ

withCrumb :: Typechecker a -> MoleculeCrumb -> Typechecker a
withCrumb action crumb = local (_1 .~ Just crumb) action

withScopedVar :: Typechecker a -> Name -> Typechecker a
withScopedVar action var = local (_2 %~ (S.insert var)) action 

withCrumbAndScopedVar :: Typechecker a -> MoleculeCrumb -> Name -> Typechecker a
withCrumbAndScopedVar action crumb var = 
  local ((_1 .~ Just crumb) . (_2 %~ S.insert var)) action 

typecheck :: MoleculeExpr -> Typechecker MoleculeType
typecheck (EVar name) = do
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
          CApp1 e -> throwError . TypeError $ name ++ " is not a function"
          CApp2 e -> do
            t <- typecheck e
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
            t' <- typecheck e
            case t of
              TLam typ _ | typ == t -> addBinding name t
                         | otherwise -> throwError . TypeError $ show e ++ " is not a function"
              _ -> throwError . TypeError $ show e ++ " is not a function" 

typecheck (e1 :+: e2) = do
  e1' <- typecheck e1 `withCrumb` CPlusA e2
  e2' <- typecheck e2 `withCrumb` CPlusB e1
  case (e1', e2') of
    (TInt, TInt) -> return TInt
    (t, TInt)    -> throwError . TypeError $ "type error in the first argument of +"
    (TInt, t)    -> throwError . TypeError $ "type error in the second argument of +"
    _            -> throwError . TypeError $ "type error in both arguments of +"

typecheck (e1 :|: e2) = do
  e1' <- typecheck e1 `withCrumb` COrA e2
  e2' <- typecheck e2 `withCrumb` COrB e1
  case (e2', e2') of
    (TBool, TBool) -> return TBool
    (t, TBool)     -> throwError . TypeError $ "type error in the first argument of |"
    (TBool, t)     -> throwError . TypeError $ "type error in the second argument of |"
    _              -> throwError . TypeError $ "type error in both arguments of |"

typecheck (EApp e1 e2) = do
  t  <- typecheck e1 `withCrumb` CApp1 e2
  t' <- typecheck e2 `withCrumb` CApp2 e1
  case t of
    TLam t1 t2 | t1 == t'  -> return t1
               | otherwise -> throwError . TypeError $ "type error in function application"
    _ -> throwError . TypeError $ "type error in function application"  

typecheck (EAbs name expr) = do
  t   <- withCrumbAndScopedVar (typecheck expr) (CAbs name) name
  env <- get
  case M.lookup name env of
    Nothing -> throwError . TypeError $ "cannot infer type of " ++ name
    Just t' -> return $ TLam t' t  

typecheck ETrue    = return TBool
typecheck EFalse   = return TBool
typecheck (EInt _) = return TInt

test = runTypecheck Nothing S.empty M.empty