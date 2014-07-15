import Typechecker
import Types
import Control.Monad
import Data.Either
import qualified Data.Map as M
import qualified Data.Set as S

testExprs :: [MoleculeExpr]
testExprs = [
    EApp (EAbs "x" (EVar "x" :+: EVar "x")) (EInt 10)
  , EAbs "x" (EVar "x" :+: EVar "x")
  , EApp (EAbs "x" (EVar "x")) (EInt 8)
  , EApp (EApp (EAbs "x" (EAbs "y" (EVar "x" :+: EVar "y"))) (EInt 10)) (EInt 10)
  , EInt 10
  , ETrue
  , EFalse
  , EApp (EAbs "x" ETrue) (EInt 10)
  ]

failExprs :: [MoleculeExpr]
failExprs = [
    ETrue :+: EInt 10
  , EAbs "a" (EVar "a" :|: EVar "b")
  , EVar "y"
  , EApp (EApp (EAbs "x" (EAbs "y" (EVar "x" :+: EVar "y"))) (EInt 10)) (ETrue)
  ]

test = do
  let rights = map typecheck testExprs
      lefts  = map typecheck failExprs
  putStrLn "rights:"
  mapM_ print rights
  putStrLn "lefts:"
  mapM_ print lefts
  guard (all isRight rights)
  guard (all isLeft lefts)
  print "tests succeeded."