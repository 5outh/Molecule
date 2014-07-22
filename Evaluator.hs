module Evaluator where

import Types

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Except
import Data.Map as M
import Data.Maybe

type Evaluator = ExceptT MoleculeError (Reader (M.Map Name MoleculeValue))

runtimeError :: String -> Evaluator a
runtimeError = throwError . RuntimeError

eval :: MoleculeExpr -> Evaluator MoleculeValue
eval e = case e of
  ETrue    -> return $ VBool True
  EFalse   -> return $ VBool False
  EInt x   -> return $ VInt x
  EVar nm  -> do
    env <- ask
    case M.lookup nm env of
      Just x -> return x
      _      -> runtimeError $ "Internal error: Failure to typecheck variable properly (" ++ nm ++ " is not in scope)"
  e1 :+: e2 -> do
    [a, b] <- mapM eval [e1, e2]
    case (a, b) of
      (VInt a', VInt b') -> return $ VInt (a' + b')
      _ -> runtimeError "Internal error: Failure to typecheck properly in +"  
  e1 :|: e2 -> do
    [a, b] <- mapM eval [e1, e2]
    case (a, b) of
      (VBool a', VBool b') -> return $ VBool (a' || b')
      _ -> runtimeError "Internal error: Failure to typecheck properly in |"   
  EAbs name e1 -> ask >>= \env -> return $ VLam env name e1
  EApp e1 e2  -> do
    env  <- ask
    expr <- eval e1
    case expr of
      VLam env' name body -> eval e2 >>= \e' -> 
        local (const $ M.insert name e' env') $ eval body
      _ -> runtimeError "Internal error: Failure to typecheck properly in function application"

evaluate :: MoleculeExpr -> Either MoleculeError MoleculeValue
evaluate = runEval M.empty
  where runEval env e = runReader (runExceptT (eval e)) env
