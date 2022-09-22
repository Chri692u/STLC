module Check where

import Syntax
import Control.Monad.Except
import Control.Monad.Reader

data TypeError = Mismatch Type Type
               | NotFunction Type
               | NotInScope Id
               deriving (Show)

type Check = ExceptT TypeError (Reader TEnv)

inEnv :: (Id, Type) -> Check a -> Check a
inEnv (x,t) = local (extend (x,t))

extend :: (Id, Type) -> TEnv -> TEnv
extend xt env = xt : env

lookupVar :: Id -> Check Type
lookupVar x = do
  env <- ask
  case lookup x env of
    Just e  -> return e
    Nothing -> throwError $ NotInScope x

typeof :: Expr -> Check Type
typeof (Lit (LInt x)) = return TInt
typeof (Lit (LBool b)) = return TBool
typeof (Lambda symbol type' body) = do
    rhs <- inEnv (symbol, type') (typeof body)
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