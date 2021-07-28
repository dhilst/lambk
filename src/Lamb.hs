{-# LANGUAGE GADTs #-}
module Lamb where

import Control.Monad.Reader
import qualified Data.Map.Strict as M
import Debug.Trace
import Test.Hspec
import Text.Show.Unicode

{-# ANN module ("hlint: ignore Use <$>") #-}

data Term
  = Var String
  | Lamb String Term
  | App Term Term
  | TBool Bool
  | TUnit
  | TBoolExpr BoolOp Term Term
  deriving (Eq)


data LType a b = Arrow a b | TVar a | Unit | Bool


data Term' t a where
  Var' :: String -> Term' t a
  Lamb' :: String -> Term' t a
  App' :: Term' t a -> Term' t a
  TBool' :: Bool -> Term' Bool a
  TUnit' :: () -> Term' () a
  TBoolExpr' :: BoolOp -> Term' Bool a  -> Term' Bool a -> Term' Bool a

-- ((λx.x) 1) || B
-- A -> bool
-- B -> bool
-- bool || bool -> bool


data BoolOp
  = And
  | Or
  deriving (Eq)

instance Show Term where
  show (Var x) = x
  show (Lamb x t) = "(λ" ++ x ++ "." ++ show t ++ ")"
  show (App t1 t2) = "(" ++ show t1 ++ " " ++ show t2 ++ ")"
  show (TBool x) =
    if x
      then "true"
      else "false"
  show TUnit = "()"
  show (TBoolExpr op a b) =
    "(" ++ show a ++ " " ++ show op ++ " " ++ show b ++ ")"

instance Show BoolOp where
  show And = "&&"
  show Or = "||"
