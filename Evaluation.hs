module Evaluation where

import Syntax

eval :: Expr -> Env -> Value
eval (Lit (LInt x))           env = Ilit x
eval (Lit (LBool b))          env = Blit b
eval (List xs)         env = IList xs'
  where xs' = map (\x -> eval x env) xs 
eval (Var symbol)             env = lookupEnv symbol env
eval (Lambda symbol _ expr)   env = Closure symbol expr env
eval (App lambda symbol)      env = apply lambda' symbol'
        where lambda' = eval lambda env
              symbol' = eval symbol env

-- Symbol table lookup
lookupEnv :: Id -> Env -> Value
lookupEnv symbol []  = error "Symbol table empty"
lookupEnv symbol env = snd $ head $ filter(\(symbol', _) -> symbol == symbol') env

-- Apply lambda abstraction
apply :: Value -> Value -> Value
apply (Closure symbol expr env) arg = eval expr ((symbol, arg):env)