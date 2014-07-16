module Evaluator where

import Types

import Control.Monad.Reader
import Control.Monad.Except
import Data.Map as M
import Data.Maybe

runtimeError = throwError . RuntimeError

eval :: MoleculeExpr -> ExceptT MoleculeError (Reader (M.Map Name MoleculeValue)) MoleculeValue
eval ETrue     = return $ VBool True
eval EFalse    = return $ VBool False
eval (EInt x)  = return $ VInt x

eval (EVar nm) = do
  env <- ask
  case M.lookup nm env of
    Just x -> return x
    _      -> runtimeError $ "Internal error: Failure to typecheck variable properly (" ++ nm ++ " is not in scope)"

eval (e1 :+: e2) = do
  a <- eval e1
  b <- eval e2
  case (a, b) of
    (VInt a', VInt b') -> return $ VInt (a' + b')
    _ -> runtimeError "Internal error: Failure to typecheck properly in +"  

eval (e1 :|: e2) = do
  a <- eval e1
  b <- eval e2
  case (a, b) of
    (VBool a', VBool b') -> return $ VBool (a' || b')
    _ -> runtimeError "Internal error: Failure to typecheck properly in |"   

eval (EAbs name e) = ask >>= \env -> return $ VLam env name e
eval (EApp e1 e2)  = do
  env <- ask
  expr <- eval e1
  case expr of
    VLam env' name body -> do
      e <- eval e2
      (local (const $ M.insert name e env')) (eval body)
    _ -> runtimeError "Internal error: Failure to typecheck properly in function application"

runEval :: Env -> MoleculeExpr -> Either MoleculeError MoleculeValue
runEval env e = runReader (runExceptT (eval e)) env

evaluate :: MoleculeExpr -> Either MoleculeError MoleculeValue
evaluate = runEval M.empty
