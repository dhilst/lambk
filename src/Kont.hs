module Kont where

import qualified Data.Map.Strict as M
import Debug.Trace
import Lambk

type Control = Term

type Env = M.Map String D

data D
  = Clo (Term, Env)
  | FVar (String, Env)
  deriving (Show)

type IState = (Control, Env, Kont)

type Program = Control

data Kont
  = Mt
  | Ar (Control, Env, Kont)
  | Fn (Term, Env, Kont)
  deriving (Show)

step :: IState -> IState
step t@(Var x, env, kont) = trace ("step Var " ++ show t) (t', env', kont)
  where
    Clo (t', env') =
      case M.lookup x env of
        Just t -> t
        Nothing -> error $ "Unbounded " ++ x
step t@(App t1 t2, env, kont) =
  trace ("step Ar " ++ show t) (t1, env, Ar (t2, env, kont))
step t@(l@(Lamb x body), env, Ar (t2, env', kont)) =
  trace ("step Fn " ++ show t) (t2, env, Fn (l, env, kont))
step t@(lam@(Lamb _ _), env, Fn (Lamb x body, env', kont)) =
  trace ("step Apply " ++ show t) (body, M.insert x (Clo (lam, env)) env, kont)

terminal :: (IState -> IState) -> (IState -> Bool) -> IState -> IState
terminal step isFinal state
  | isFinal state = state
  | otherwise = terminal step isFinal $ step state

isFinal :: IState -> Bool
isFinal (Lamb _ _, env, Mt) = True
isFinal _ = False

inject :: Program -> IState
inject expr = (expr, M.empty, Mt)

cekEval :: Program -> IState
cekEval pr = terminal step isFinal $ inject pr
