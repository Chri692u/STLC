module Syntax where

type Id = String

data Expr = Nat Int
    | Let Id Expr Expr
    | Var Id
    | Lambda Id Expr
    | App Expr Expr
    | Binary Primitive Expr Expr
    deriving (Show)

data Type = TNat
    | TArr Type Type

data Primitive = Add
    | Sub
    | Mul
    | Div
    deriving (Show)

data Value = Ilit Int
    | Closure Id Expr Env

instance Show Value where
    show (Ilit x) = show x
    show Closure{} = "<closure>"

type Env = [(Id, Value)]
