data MoleculeType = 
    TBool Bool
  | TInt Int
    deriving (Show, Eq)

data MoleculeExpr = 
    EVar String
  | ETrue | EFalse
  | EInt Int
  | EIf MoleculeExpr MoleculeExpr MoleculeExpr
  | ELet String MoleculeExpr MoleculeExpr
  | MoleculeExpr :+: MoleculeExpr
    deriving (Show, Eq)

data ASTCrumb = 
    CIfPred MoleculeExpr MoleculeExpr -- Came from predicate
  | CIfThen MoleculeExpr MoleculeExpr -- Came from "then"
  | CIfElse MoleculeExpr MoleculeExpr -- Came from "else"
  | CPlusA MoleculeExpr               -- Came from (a +)
  | CPlusB MoleculeExpr               -- Came from (+ b)
  | CLetBe String MoleculeExpr        -- Came from Let x be (expr) in ...
  | CLetIn String MoleculeExpr        -- Came from Let x be ... in (expr)
    deriving (Show, Eq)

data ASTZipper = ASTZipper MoleculeExpr [ASTCrumb]
  deriving (Show, Eq)

testAST :: MoleculeExpr
testAST = EIf (EVar "a") ETrue (EIf ETrue (EInt 0) (EInt 10 :+: EInt 10))