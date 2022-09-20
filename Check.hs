module Check where

import Syntax
import Control.Monad.Except

type Check a = Except TError a
data TError = TMismatch Type Type

typeof :: Expr -> Either TError Type
