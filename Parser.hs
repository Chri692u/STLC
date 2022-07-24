module Parser (parseExpr) where

import Data.Char
import Text.Parsec
import Text.Parsec.String(Parser)
import Text.Parsec.Language(haskellStyle)

import qualified Text.Parsec.Expr as X
import qualified Text.Parsec.Token as T

import Syntax

lexer :: T.TokenParser ()
lexer = T.makeTokenParser style
  where ops = ["->","\\","+","*","-","="]
        names = []
        style = haskellStyle {T.reservedOpNames = ops,
                              T.reservedNames = names,
                              T.commentLine = "#"}

reserved :: String -> Parser ()
reserved = T.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = T.reservedOp lexer

identifier :: Parser String
identifier = T.identifier lexer

parens :: Parser a -> Parser a
parens = T.parens lexer

content :: Parser a -> Parser a
content p = do
  T.whiteSpace lexer
  r <- p
  eof
  return r
