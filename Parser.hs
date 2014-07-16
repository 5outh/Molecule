module Parser where

import Types

import Text.ParserCombinators.Parsec hiding ((<|>))
import qualified Text.Parsec.Token as T
import Text.Parsec.Language
import Text.Parsec.Expr
import Control.Applicative

moleculeDef :: LanguageDef st
moleculeDef = emptyDef
  { T.opLetter = oneOf "+|"
  , T.reservedOpNames = words "+ | \\ ."
  , T.reservedNames  = words "t f"
  }

molecule :: T.TokenParser st
molecule = T.makeTokenParser moleculeDef

identifier = T.identifier molecule
reservedOp = T.reservedOp molecule
reserved   = T.reserved molecule
integer    = T.integer molecule
whiteSpace = T.whiteSpace molecule
parens     = T.parens molecule

binary name label assoc = Infix (reservedOp name *> pure (\x y -> (label x y))) assoc

opTable = [
  [ binary "+" (:+:) AssocLeft 
  , binary "|" (:|:) AssocLeft]
  , [app] ]

app = Infix space AssocLeft
  where space = whiteSpace
          *> notFollowedBy (choice . map reservedOp $ T.reservedOpNames moleculeDef)
          *> pure (\x y -> EApp x y)

value :: Parser MoleculeExpr
value = parens expression 
    <|> lambda 
    <|> int
    <|> true
    <|> false
    <|> var

expression :: Parser MoleculeExpr
expression = buildExpressionParser opTable value

lambda :: Parser MoleculeExpr
lambda = do
  reservedOp "\\"
  name <- identifier
  reservedOp "."
  body <- liftA (foldr1 EApp) (many1 expression)
  return $ EAbs name body

int :: Parser MoleculeExpr
int = liftA (EInt . fromInteger) integer

true :: Parser MoleculeExpr
true = reserved "t" *> pure ETrue

false :: Parser MoleculeExpr
false = reserved "f" *> pure EFalse

var :: Parser MoleculeExpr
var = liftA EVar identifier

parseMolecule :: String -> Either MoleculeError MoleculeExpr
parseMolecule e = case parse expression "(molecule)" e of
  Left err    -> Left . ParseError $ show err
  Right e     -> Right e