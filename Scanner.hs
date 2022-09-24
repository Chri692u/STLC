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
  where ops = ["\\", ".", ":", "[", "]" ]
        names = ["apply", "Bool", "Int", "True", "List", "False"]
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

int :: Parser Integer
int = T.integer scanner

variable :: Parser Expr
variable = do
    x <- identifier
    return (Var x)

number :: Parser Expr
number = do
    n <- int
    return (Lit (LInt (fromIntegral n)))    

bool :: Parser Expr
bool = (reserved "True" >> return (Lit (LBool True)))
     <|> (reserved "False" >> return (Lit (LBool False)))

list :: Parser Expr
list = do
    reservedOp "["
    xs <- many (number <|> bool)
    reservedOp "]"
    return (List xs)

lambda :: Parser Expr
lambda = do
    reservedOp "\\"
    arg <- identifier
    reservedOp ":"
    type' <- types
    reservedOp "."
    body <- expr
    return (Lambda arg type' body)

apply :: Parser Expr
apply = do
    reserved "apply"
    abs <- expr
    arg <- expr
    return (App abs arg)

tAtom :: Parser Type
tAtom = tLit <|> (parens types)

tLit :: Parser Type
tLit = (reservedOp "Bool" >> return TBool)
     <|> (reservedOp "Int" >> return TInt)
     <|> (reservedOp "[Int]" >> return (TList TInt))
     <|> (reservedOp "[Bool]" >> return (TList TBool))

types :: Parser Type
types = X.buildExpressionParser tyops tAtom
    where 
        infixOp x f = X.Infix(reservedOp x >> return f)
        tyops = [ [infixOp "->" TArr X.AssocRight] ]

term :: Parser Expr
term = parens expr
       <|> variable
       <|> number
       <|> bool
       <|> list
       <|> lambda
       <|> apply

content :: Parser a -> Parser a
content p = do
    T.whiteSpace scanner
    r <- p
    eof
    return r

expr :: Parser Expr
expr = X.buildExpressionParser [] term

parseExpr :: String -> Either ParseError Expr
parseExpr input = parse (content expr) "<stdin>" input