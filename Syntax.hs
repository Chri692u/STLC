module Syntax where

type Id = String

data Expr = Lit Base
    | List [Expr]
    | Var Id
    | Lambda Id Type Expr
    | App Expr Expr
    deriving (Show)

data Base = LInt Int
    | LBool Bool
    deriving (Show)

data Type = TInt
    | TBool
    | TList Type
    | TArr Type Type
    deriving(Show, Read, Eq)

data Value = Ilit Int
    | Blit Bool
    | IList [Value] 
    | Closure Id Expr Env

instance Show Value where
    show (Ilit x) = show x
    show (Blit x) = show x
    show (IList x) = show x
    show Closure{} = "<<closure>>"

type Env = [(Id, Value)]
type TEnv = [(Id, Type)]