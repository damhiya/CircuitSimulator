{-#
  LANGUAGE
  GADTs,
  KindSignatures
#-}

module Expression where

import Data.Hashable
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import qualified Data.Set as S

class (Eq v, Ord v, Hashable v) => Variable v

type State v = HM.HashMap v Double

data Expression :: * -> * where
  Zero  :: Variable v => Expression v
  One   :: Variable v => Expression v
  Const :: Variable v => Double -> Expression v
  Var   :: Variable v => v -> Expression v
  Add   :: Variable v => Expression v -> Expression v -> Expression v
  Sub   :: Variable v => Expression v -> Expression v -> Expression v
  Mul   :: Variable v => Expression v -> Expression v -> Expression v
  Div   :: Variable v => Expression v -> Expression v -> Expression v
  Neg   :: Variable v => Expression v -> Expression v
  Inv   :: Variable v => Expression v -> Expression v

relatedVariables :: Variable v => Expression v -> S.Set v
relatedVariables Zero       = S.empty
relatedVariables One        = S.empty
relatedVariables (Const _)  = S.empty
relatedVariables (Var x)    = S.singleton x
relatedVariables (Add x y)  = S.union (relatedVariables x) (relatedVariables y)
relatedVariables (Sub x y)  = S.union (relatedVariables x) (relatedVariables y)
relatedVariables (Mul x y)  = S.union (relatedVariables x) (relatedVariables y)
relatedVariables (Div x y)  = S.union (relatedVariables x) (relatedVariables y)
relatedVariables (Neg x)    = relatedVariables x
relatedVariables (Inv x)    = relatedVariables x

collapse :: Expression v -> Expression v
collapse e = case e of
  Zero      -> Zero
  One       -> One
  Const x   -> Const x
  Var x     -> Var x
  Add e1 e2 -> collapse' (Add (collapse e1) (collapse e2))
  Sub e1 e2 -> collapse' (Sub (collapse e1) (collapse e2))
  Mul e1 e2 -> collapse' (Mul (collapse e1) (collapse e2))
  Div e1 e2 -> collapse' (Div (collapse e1) (collapse e2))
  Neg e     -> collapse' (Neg (collapse e))
  Inv e     -> collapse' (Inv (collapse e))
  where
    collapse' (Add e Zero) = e
    collapse' (Add Zero e) = e
    collapse' (Sub e Zero) = e
    collapse' (Sub Zero e) = collapse' (Neg e)
    collapse' (Mul Zero e) = Zero
    collapse' (Mul e Zero) = Zero
    collapse' (Mul One e) = e
    collapse' (Mul e One) = e
    collapse' (Mul (Const x) (Const y)) = Const (x*y)
    collapse' (Div e Zero) = undefined
    collapse' (Div Zero e) = Zero
    collapse' (Neg (Neg e)) = e
    collapse' (Neg (Const x)) = Const (-x)
    collapse' (Neg Zero) = Zero
    collapse' (Inv (Inv e)) = e
    collapse' (Inv Zero) = undefined
    collapse' e = e

partial :: Variable v => v -> Expression v -> Expression v
partial _ Zero = Zero
partial _ One  = Zero
partial _ (Const _) = Zero
partial x (Var v)  | v == x    = One
                | otherwise = Zero
partial x (Add e1 e2) = Add (partial x e1) (partial x e2)
partial x (Sub e1 e2) = Sub (partial x e1) (partial x e2)
partial x (Mul e1 e2) = Add (Mul (partial x e1) e2) (Mul e1 (partial x e2))
partial x (Div e1 e2) = partial x (Mul e1 (Inv e2))
partial x (Neg e) = Neg (partial x e)
partial x (Inv e) = Neg (Div (partial x e) (Mul e e))

grad :: Variable v => [v] -> Expression v -> [Expression v]
grad vars exp = map (\var -> partial var exp) vars

jacobian :: Variable v => [v] -> [Expression v] -> [[Expression v]]
jacobian vars exps = map (grad vars) exps

evaluate :: Variable v => State v -> Expression v -> Double
evaluate _ Zero = 0.0
evaluate _ One  = 1.0
evaluate _ (Const x)  = x
evaluate s (Var v) = fromJust $ HM.lookup v s
evaluate s (Add e1 e2) = (evaluate s e1) + (evaluate s e2)
evaluate s (Sub e1 e2) = (evaluate s e1) - (evaluate s e2)
evaluate s (Mul e1 e2) = (evaluate s e1) * (evaluate s e2)
evaluate s (Div e1 e2) = (evaluate s e1) / (evaluate s e2)
evaluate s (Neg e) = negate (evaluate s e)
evaluate s (Inv e) = 1.0  / (evaluate s e)

partialEvaluate :: Eq a => State a -> Expression a -> Expression a
partialEvaluate _ Zero = Zero
partialEvaluate _ One  = One
partialEvaluate _ (Const x) = Const x
partialEvaluate s (Var v) = case HM.lookup v s of
  Just val -> Const val
  Nothing  -> Var v
partialEvaluate s (Add e1 e2) = Add (partialEvaluate s e1) (partialEvaluate s e2)
partialEvaluate s (Sub e1 e2) = Sub (partialEvaluate s e1) (partialEvaluate s e2)
partialEvaluate s (Mul e1 e2) = Mul (partialEvaluate s e1) (partialEvaluate s e2)
partialEvaluate s (Div e1 e2) = Div (partialEvaluate s e1) (partialEvaluate s e2)
partialEvaluate s (Neg e) = Neg (partialEvaluate s e)
partialEvaluate s (Inv e) = Inv (partialEvaluate s e)

data Equation v = Equation {lhs :: Expression v}

instance (Show v) => Show (Expression v) where
  show Zero = "0"
  show One  = "1"
  show (Const x) = show x
  show (Var v)   = show v
  show (Add e1 e2) = "(Add " ++ (show e1) ++ " " ++ (show e2) ++ ")"
  show (Sub e1 e2) = "(Sub " ++ (show e1) ++ " " ++ (show e2) ++ ")"
  show (Mul e1 e2) = "(Mul " ++ (show e1) ++ " " ++ (show e2) ++ ")"
  show (Div e1 e2) = "(Div " ++ (show e1) ++ " " ++ (show e2) ++ ")"
  show (Neg e) = "(Neg " ++ (show e) ++ ")"
  show (Inv e) = "(Inv " ++ (show e) ++ ")"

instance (Show v) => Show (Equation v) where
  show (Equation exp) = (show exp) ++ " == 0"
