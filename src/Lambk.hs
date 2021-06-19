{-# LANGUAGE QuasiQuotes #-}
module Lambk where

import           Control.Monad.Reader
import qualified Data.Map.Strict         as M
import           Data.String.Interpolate (i)
import           Debug.Trace
import           Test.Hspec
import           Text.Show.Unicode
{-# ANN module ("hlint: ignore Use <$>") #-}



data Term
  = Var String
  | FVar String
  | Lamb String Term
  | App Term Term
  deriving (Eq)


instance Show Term where
  show (Var x)     = x
  show (Lamb x t)  = "(λ" ++ x  ++ "." ++ show t ++ ")"
  show (App t1 t2) = [i|(#{show t1} #{show t2})|]
  show (FVar x)    = x


isVal :: Term -> Bool
isVal (Lamb _ _) =  True
isVal (FVar _) = True
isVal (Var _) = False
isVal (App t1 _) = case t1 of
  Lamb _ _ -> False
  _        -> True



type Env = M.Map String Term

eval :: Term -> Term
eval t = eval' t M.empty
  where eval' :: Term -> Env -> Term
        eval' t env
          | isVal t = t
          | otherwise = case t of
              Var v -> case M.lookup v env of
                Just t  -> t
                Nothing -> FVar v
              App t1 t2 ->
                case t1 of
                  Lamb x body -> let v = eval' t2 env
                                 in eval' v $ M.insert x v env
                  _           -> t
