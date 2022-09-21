module Evaluation where

import Syntax

-- Evaluation of literals, variables, let-in bindings, lambda abstractions and lambda application
eval :: Expr -> Env -> Value
eval (Lit (LInt x))           env = Ilit x
eval (Lit (LBool b))          env = Blit b
eval (Var symbol)             env = findSymbol symbol env
eval (Let symbol expr1 expr2) env = eval expr2 (expandEnv symbol expr1 env)
eval (Lambda symbol _ expr)     env = Closure symbol expr env
eval (App lambda symbol)      env = apply lambda' symbol'
        where lambda' = eval lambda env
              symbol' = eval symbol env
eval (Binary op expr1 expr2) env = (evalOp op) (eval expr1 env) (eval expr2 env)

-- Evaluation of binary operations
evalOp :: Primitive -> Value -> Value -> Value
evalOp Add (Ilit expr1) (Ilit expr2) = Ilit (expr1 + expr2)
evalOp Sub (Ilit expr1) (Ilit expr2) = Ilit (expr1 - expr2)
evalOp Mul (Ilit expr1) (Ilit expr2) = Ilit (expr1 * expr2)
evalOp Div (Ilit expr1) (Ilit expr2) = Ilit (expr1 `div` expr2)
-- Aux
-- Symbol table lookup
findSymbol :: Id -> Env -> Value
findSymbol symbol []  = error "Symbol table empty"
findSymbol symbol env = snd $ head $ filter(\(symbol', _) -> symbol == symbol') env

-- Symbol table expansion
expandEnv :: Id -> Expr -> Env -> Env
expandEnv symbol expr env = (symbol, eval expr env):env

-- Apply lambda abstraction
apply :: Value -> Value -> Value
apply (Closure symbol expr env) arg = eval expr ((symbol, arg):env)
apply _ _ = undefined
