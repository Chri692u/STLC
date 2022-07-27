module Scanner (parseExpr) where

import Data.Char
import Text.Parsec
import Text.Parsec.String(Parser)
import Text.Parsec.Language(haskellStyle)

import qualified Text.Parsec.Expr as X
import qualified Text.Parsec.Token as T

import Syntax

scanner :: T.TokenParser ()
scanner = T.makeTokenParser style
  where ops = ["\\", "=", ".", "+"]
        names = ["let", "in", "apply"]
        style = haskellStyle {T.reservedOpNames = ops,
                              T.reservedNames = names,
                              T.commentLine = "#"}

reserved :: String -> Parser ()
reserved = T.reserved scanner

reservedOp :: String -> Parser ()
reservedOp = T.reservedOp scanner

identifier :: Parser String
identifier = T.identifier scanner

parens :: Parser a -> Parser a
parens = T.parens scanner

content :: Parser a -> Parser a
content p = do
    T.whiteSpace scanner
    r <- p
    eof
    return r
  
nat :: Parser Integer
nat = T.natural scanner
  
variable :: Parser Expr
variable = do
    x <- identifier
    return (Var x)

number :: Parser Expr
number = do
    n <- nat
    return (Inum (fromIntegral n))
    
letIn :: Parser Expr
letIn = do
    reserved "let"
    x <- identifier
    reservedOp "="
    binding <- expr
    reserved "in"
    rest <- expr  
    return (Let x binding rest)
    
lambda :: Parser Expr
lambda = do
    reservedOp "\\"
    arg <- identifier
    reservedOp "."
    body <- expr
    return (Lambda arg body)
    
apply :: Parser Expr
apply = do
    reserved "apply"
    abs <- expr
    arg <- expr
    return (App abs arg)
    
term :: Parser Expr
term = parens expr
       <|> letIn
       <|> variable
       <|> number
       <|> lambda
       <|> apply

expr :: Parser Expr
expr = X.buildExpressionParser table term

table = [
    [X.Infix (reservedOp "+" >> return (Binary Add)) X.AssocLeft]
    ]
            
parseExpr :: String -> Either ParseError Expr
parseExpr input = parse (content expr) "<stdin>" input
