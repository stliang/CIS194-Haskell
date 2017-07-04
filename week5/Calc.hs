{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Calc where

import Data.Maybe
import ExprT
import Parser (parseExp)
import StackVM -- (StackExp)

eval :: ExprT -> Integer
eval (ExprT.Lit i) = i
eval (ExprT.Add exp1 exp2) = eval exp1 + eval exp2
eval (ExprT.Mul exp1 exp2) = eval exp1 * eval exp2

evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp ExprT.Lit ExprT.Add ExprT.Mul

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = ExprT.Lit
  add = ExprT.Add
  mul = ExprT.Mul

reify :: ExprT -> ExprT
reify = id

instance Expr Bool where
  lit x = x > 0
  add = (||)
  mul = (&&)

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

newtype MinMax =
  MinMax Integer
  deriving (Eq, Show)

instance Expr MinMax where
  lit = MinMax
  add (MinMax x) (MinMax y) = MinMax (max x y)
  mul (MinMax x) (MinMax y) = MinMax (min x y)

newtype Mod7 =
  Mod7 Integer
  deriving (Eq, Show)

instance Expr Mod7 where
  lit x = Mod7 (mod x 7)
  add (Mod7 x) (Mod7 y) = Mod7 (mod (x + y) 7)
  mul (Mod7 x) (Mod7 y) = Mod7 (mod (x * y) 7)

testExp
  :: Expr a
  => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer

testBool = testExp :: Maybe Bool

testMM = testExp :: Maybe MinMax

testSat = testExp :: Maybe Mod7

instance Expr StackVM.Program where
  lit i = [StackVM.PushI i]
  add a b = a ++ b ++ [StackVM.Add]
  mul a b = a ++ b ++ [StackVM.Mul]

testProg :: Maybe StackVM.Program
testProg = testExp

compile2 :: String -> Either String StackVal
compile2 = stackVM . fromMaybe [] . compile

compile :: String -> Maybe Program
compile = parseExp lit add mul

class HasVars a where
  var :: String -> a

data VarExprT =
  VarExprT String
           Integer
  deriving (Show, Eq)

instance Expr VarExprT where
  lit = VarExprT ""
  add (VarExprT _ a) (VarExprT _ b) = VarExprT "" (a + b)
  mul (VarExprT _ a) (VarExprT _ b) = VarExprT "" (a * b)

instance HasVars VarExprT where
  var str = VarExprT str 0

type MapExpr = M.Map String Integer -> Maybe Integer

instance HasVars MapExpr where
  var = M.lookup

instance Expr MapExpr where
  lit a = (\_ -> Just a)
  add f g =
    \m ->
      case (isNothing (f m) || isNothing (g m)) of
        True -> Nothing
        _ -> Just (fromJust (f m) + fromJust (g m))
  mul f g =
    \m ->
      case (isNothing (f m) || isNothing (g m)) of
        True -> Nothing
        _ -> Just (fromJust (f m) * fromJust (g m))

withVars :: [(String, Integer)] -> MapExpr -> Maybe Integer
withVars vs expr = expr $ M.fromList vs
