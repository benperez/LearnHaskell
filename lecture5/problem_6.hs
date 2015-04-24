{-# LANGUAGE FlexibleInstances #-}

import Control.Applicative
import Parser
import qualified Data.Map as M

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

data VarExprT = Lit Integer
              | Add VarExprT VarExprT
              | Mul VarExprT VarExprT
              | Var String
                deriving (Eq, Show)

instance Expr VarExprT where
  lit = Lit
  add = Add
  mul = Mul

class HasVars a where
  var :: String -> a

instance HasVars VarExprT where
  var = Var

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit x = (\_ -> Just x)
  add e1 e2 m = (+) <$> e1 m <*> e2 m
  mul e1 e2 m = (*) <$> e1 m <*> e2 m

withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs exp = exp $ M.fromList vs
