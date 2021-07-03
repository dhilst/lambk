module CEK where

import qualified Data.Map.Strict as M
import Debug.Trace
import Lamb

type Control = Term

type Env = M.Map String D

data D
  = Clo (Term, Env)
  | Const Term
  deriving (Show)

type IState = (Control, Env, Kont)

type Program = Control

data Kont
  = Mt
  | Ar (Control, Env, Kont)
  | Fn (Term, Env, Kont)
  deriving (Show)

-- e ::= v | x | (e e)
-- v ::= c | (λx.e)
--
-- c ::= true | false | ()
--
-- E ::= [] | (E e) | (v E)
--
--    (sub1 ((lambda (x) (add 1 x) 1)))       ~ E = (sub1 [])
-- -> (sub1 (add1 x)[1/x]) = (sub1 (add1 1))  ~ E = (sub1 [])
-- -> (subl 2)                                ~ E = []
-- -> 1
--
-- -> ((λx.x) true)   ~ E = ([] true)
-- -> ((λx.x) true)   ~ E = ((λx.x) [])
-- -> ((λx.x) true)   ~ E = [((λx.x) true)] :> x[true/x] :> x
-- -> x               ~ E = []
step :: IState -> IState
step t@(TUnit, env, Fn (Lamb x body, env', kont)) =
  trace ("step Unit " ++ show t) (body, M.insert x (Const TUnit) env, kont)
step t@(b@(TBool _), env, Fn (Lamb x body, env', kont)) =
  trace ("step Bool " ++ show t) (body, M.insert x (Const b) env, kont)
--step t@(t'@TUnit, env, kont) = trace ("step Unit " ++ show t) (t', env, kont)
step t@(Var x, env, kont) = trace ("step Var " ++ show t) lookup x
  where
    lookup x =
      case M.lookup x env of
        Just t ->
          case t of
            Clo (t', env') -> (t', env', kont)
            Const c -> (c, env, kont)
        Nothing -> error $ "Unbounded " ++ x
step t@(App t1 t2, env, kont) =
  trace ("step Eval t1, K = Ar(t2) : " ++ show t) (t1, env, Ar (t2, env, kont))
step t@(l@(Lamb x body), env, Ar (t2, env', kont)) =
  trace ("step Eval t2, K = Fn(t1) : " ++ show t) (t2, env, Fn (l, env, kont))
step t@(lam@(Lamb _ _), env, Fn (Lamb x body, env', kont)) =
  trace
    ("step Apply (t1 t2) : " ++ show t)
    (body, M.insert x (Clo (lam, env)) env, kont)

terminal :: (IState -> IState) -> (IState -> Bool) -> IState -> IState
terminal step isFinal state
  | isFinal state = state
  | otherwise = terminal step isFinal $ step state

isFinal :: IState -> Bool
isFinal (TBool _, env, Mt) = True
isFinal (TUnit, env, Mt) = True
isFinal (Lamb _ _, env, Mt) = True
isFinal _ = False

inject :: Program -> IState
inject expr = (expr, M.empty, Mt)

cekEval :: Program -> Term
cekEval pr =
  let (control, _, _) = terminal step isFinal $ inject pr
   in control
