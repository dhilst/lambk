module Lambk where

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
  deriving (Eq)

instance Show Term where
  show (Var x) = x
  show (Lamb x t) = "(λ" ++ x ++ "." ++ show t ++ ")"
  show (App t1 t2) = "(" ++ show t1 ++ " " ++ show t2 ++ ")"

isVal :: Term -> Bool
isVal (Lamb _ _) = True
isVal (Var _) = True
isVal (App t1 _) =
  case t1 of
    Lamb _ _ -> False
    _ -> True

eval :: Term -> Term
eval t
  | isVal t = t
  | otherwise =
    case t of
      App t1 t2 ->
        case t1 of
          Lamb x body ->
            let v2 = eval t2
             in subs x v2 body
  where
    subs :: String -> Term -> Term -> Term
    subs x replace expr =
      case expr of
        Var y
          | x == y -> replace
        (Lamb v body)
          | v /= x -> Lamb v (subs x replace body)
        (App t1 t2) -> App (subs x replace t1) (subs x replace t2)
        _ -> expr
