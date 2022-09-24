module Check where

import Syntax
import Control.Monad.Except
import Control.Monad.Reader

data TypeError = Mismatch Type Type
               | NotFunction Type
               | MixedList Type Type
               | NotInScope Id

type Check = ExceptT TypeError (Reader TEnv)

instance Show TypeError where
  show (Mismatch t1 t2)  = "Type mismatch: type (" ++ show t1 ++ ") is not of type: (" ++ show t2 ++ ")"
  show (NotFunction t)  = "Type (" ++ show t ++ ") is not a function. Lambda application can only be done on abstractions"
  show (NotInScope v1)   = "Variable (" ++ show v1 ++ ") is not in scope"
  show (MixedList t1 t2) = "List cannot have mixed types: " ++ show t1 ++ "and: " ++ show t2

lookupTEnv :: (Id, Type) -> Check a -> Check a
lookupTEnv (x,t) = local (extendTEnv (x,t))

extendTEnv :: (Id, Type) -> TEnv -> TEnv
extendTEnv xt env = xt : env

lookupVar :: Id -> Check Type
lookupVar x = do
  env <- ask
  case lookup x env of
    Just e  -> return e
    Nothing -> throwError $ NotInScope x

typeof :: Expr -> Check Type
typeof (Lit (LInt x))   = return TInt
typeof (Lit (LBool b))  = return TBool
typeof (List (x:[])) = do
  t <- typeof x
  return (TList t)

typeof (List (x:y:[])) = do
  t1 <- typeof x
  t2 <- typeof y
  case t1==t2 of
    True  -> return (TList t1)
    False -> throwError $ (MixedList t1 t2)

typeof (List (x:xs)) = do
  t1 <- typeof x
  t2 <- typeof (head $ xs)
  case t1 == t2 of
    True  -> typeof (List xs)
    False -> throwError $ (MixedList t1 t2)

typeof (Lambda symbol type' body) = do
    rhs <- lookupTEnv (symbol, type') (typeof body)
    return (TArr type' rhs) 

typeof (App e1 e2) = do
    t1 <- typeof e1
    t2 <- typeof e2
    case t1 of
       (TArr a b) | a == t2 -> return b
                  | otherwise -> throwError $ Mismatch t2 a
       ty -> throwError $ NotFunction ty

typeof (Var symbol) = lookupVar symbol

runCheck :: TEnv -> Check a -> Either TypeError a
runCheck env = flip runReader env . runExceptT

checkTop :: TEnv -> Expr -> Either TypeError Type
checkTop env x = runCheck env $ (typeof x)