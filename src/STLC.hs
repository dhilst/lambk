{-# LANGUAGE GADTs, DataKinds, TypeInType, TypeOperators,
  RankNTypes, TypeApplications #-}

module STLC where

import qualified Data.Map as M
import Data.Maybe
import Type.Reflection hiding (App)

data T
  = I -- int tyep
  | B -- bool type
  | U -- unit type
  | N -- void type, N is for never
  | T :=> T -- arrow type
  | T :*: T -- product type
  | T :+: T -- sum type
  deriving (Show, Typeable)

data VDecl (t :: T) -- var declaration
      where
  V :: forall t. Char -> VDecl t

data Term (t :: T)
  -- Literals
      where
  BLit :: Bool -> Term B
  ILit :: Int -> Term I
  ULit :: () -> Term U
  --  Variable | Lambda | Application
  Var :: VDecl a -> Term a
  Lam :: VDecl a -> Term b -> Term (a :=> b)
  App :: Term (a :=> b) -> Term a -> Term b
  -- Product
  Prod :: Term a -> Term b -> Term (a :*: b)
  Fst :: Term (a :*: b) -> Term a
  Snd :: Term (a :*: b) -> Term b
  -- Sum
  InL :: forall b a. Term a -> Term (a :+: b)
  InR :: forall a b. Term b -> Term (a :+: b)
  Case :: Term (a :+: b) -> Term c -> Term c -> Term c

instance Show (Term t) where
  show (BLit v) = show v ++ ":" ++ "Bool"
  show (ILit v) = show v ++ ":" ++ "Int"
  show (Var (V x)) = show x
  show (Lam (V x) body) = "(" ++ show x ++ " -> " ++ show body ++ ")"
  show (App t1 t2) = "(" ++ show t1 ++ " " ++ show t2 ++ ")"
  show (Prod t1 t2) = show t1 ++ " * " ++ show t2
  show (Fst p@(Prod t1 t2)) = show "Fst " ++ show p
  show (Snd p@(Prod t1 t2)) = show "Fst " ++ show p
  show (InL a) = "InL " ++ show a
  show (InR a) = "InR " ++ show a
  show (Case a b c) = "Case " ++ show a ++ "|" ++ show b ++ "|" ++ show c

-- untyped expression
data UE =
  forall a. UE (Term a)

instance Show UE where
  show (UE a) = show a

type Env = M.Map Char UE

isVal :: Term a -> Bool
isVal (ULit _) = True
isVal (BLit _) = True
isVal (ILit _) = True
isVal (Lam _ _) = True
isVal (Prod _ _) = True
isVal (InL _) = True
isVal (InR _) = True
isVal _ = False

eval :: UE -> Env -> UE
-- Literal evaluation
eval u@(UE t) env
  | isVal t = u
-- Variable lookp
eval (UE (Var (V v))) env = fromJust $ M.lookup v env
-- Application
eval (UE (App (Lam (V v) body) arg)) env =
  let arg' = eval (UE arg) env
   in eval (UE body) (M.insert v arg' env)
-- Product elimination
eval (UE (Fst (Prod a b))) env = eval (UE a) env
eval (UE (Snd (Prod a b))) env = eval (UE b) env
-- Sum elimination
eval (UE (Case (InL a) b _)) env = eval (UE b) env
eval (UE (Case (InR a) _ c)) env = eval (UE c) env

env = M.fromList [('x', UE (BLit True)), ('y', UE (ILit 1))]

prog1 = eval (UE (Var (V 'x'))) env

prog2 = eval (UE (App (Lam (V 'x') (Var (V 'x'))) (ILit 1))) env

prog3 =
  eval (UE (App (Lam (V 'x') (Var (V 'x'))) (Lam (V 'y') (Var (V 'y'))))) env

prog4 = App (Lam (V @B 'x') (Var (V 'x'))) (BLit True)
