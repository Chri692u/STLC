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
           
instance Show Value where
    show (Ilit x) = show x
    show Closure{} = "<<closure>>"

type Env = [(Id, Value)]
