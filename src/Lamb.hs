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
  | TBoolOp Term BoolOp Term
  deriving (Eq)

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
  show (TBoolOp a op b) = show a ++ " " ++ show op ++ " " ++ show b

instance Show BoolOp where
  show And = "&&"
  show Or = "||"
