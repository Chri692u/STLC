module Syntax where

type Id = String

data Expr = Lit Base
    | Let Id Expr Expr
    | Var Id
    | Lambda Id Type Expr
    | App Expr Expr
    | Binary Primitive Expr Expr
    deriving (Show)

data Base = LInt Int
    | LBool Bool
    deriving (Show)

data Type = TInt
    | TBool
    | TArr Type Type
    deriving(Show, Read, Eq)

data Primitive = Add
    | Sub
    | Mul
    | Div
    deriving (Show)

data Value = Ilit Int
    | Blit Bool
    | Closure Id Expr Env

instance Show Value where
    show (Ilit x) = show x
    show (Blit x) = show x
    show Closure{} = "<closure>"

-- todo: make 1 single envoirnment
type Env = [(Id, Value)]
type TEnv = [(Id, Type)]
