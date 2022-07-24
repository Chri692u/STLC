module Syntax where

type Id = String

data Expr = Inum Int
          | Let Id Expr Expr
          | Var Id
          | Lambda Id Expr
          | App Expr Expr
          | Binary Operation Expr Expr
          deriving (Show)

data Operation = Add
           deriving (Show)
           
data Value = Ilit Int
           | Closure Id Expr Env
           deriving (Show)

type Env = [(Id, Value)]
