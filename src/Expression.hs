module Expression where

import Data.Maybe
import qualified Data.Set as S

data Variable a = Variable a deriving (Eq, Ord)

type State a = [(Variable a, Double)]

data Expression a = Zero
                  | One
                  | Const Double
                  | Var (Variable a)
                  | Add (Expression a) (Expression a)
                  | Sub (Expression a) (Expression a)
                  | Mul (Expression a) (Expression a)
                  | Div (Expression a) (Expression a)
                  | Neg (Expression a)
                  | Inv (Expression a)
                  deriving Show

relatedVariables :: Ord a => Expression a -> S.Set (Variable a)
relatedVariables (Add x y) = S.union (relatedVariables x) (relatedVariables y)
relatedVariables (Sub x y) = S.union (relatedVariables x) (relatedVariables y)
relatedVariables (Mul x y) = S.union (relatedVariables x) (relatedVariables y)
relatedVariables (Div x y) = S.union (relatedVariables x) (relatedVariables y)
relatedVariables (Neg x) = relatedVariables x
relatedVariables (Inv x) = relatedVariables x
relatedVariables (Var x) = S.singleton x
relatedVariables (Const _)  = S.empty
relatedVariables Zero       = S.empty

collapse :: Expression a -> Expression a
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

partial :: Eq a => Variable a -> Expression a -> Expression a
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

grad :: Eq a => [Variable a] -> Expression a -> [Expression a]
grad vars exp = map (\var -> partial var exp) vars

jacobian :: Eq a => [Variable a] -> [Expression a] -> [[Expression a]]
jacobian vars exps = map (grad vars) exps

getValue :: Eq a => State a -> Variable a -> Maybe Double
getValue [] _ = Nothing
getValue ((x,val):xs) var
  | x == var  = Just val
  | otherwise = getValue xs var

evaluate :: Eq a => State a -> Expression a -> Double
evaluate _ Zero = 0.0
evaluate _ One  = 1.0
evaluate _ (Const x)  = x
evaluate s (Var v) = fromJust $ getValue s v
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
partialEvaluate s (Var v) = case getValue s v of
  Just val -> Const val
  Nothing  -> Var v
partialEvaluate s (Add e1 e2) = Add (partialEvaluate s e1) (partialEvaluate s e2)
partialEvaluate s (Sub e1 e2) = Sub (partialEvaluate s e1) (partialEvaluate s e2)
partialEvaluate s (Mul e1 e2) = Mul (partialEvaluate s e1) (partialEvaluate s e2)
partialEvaluate s (Div e1 e2) = Div (partialEvaluate s e1) (partialEvaluate s e2)
partialEvaluate s (Neg e) = Neg (partialEvaluate s e)
partialEvaluate s (Inv e) = Inv (partialEvaluate s e)

data Equation a = Equation {expression :: Expression a}

instance Show a => Show (Variable a) where
  show (Variable id) = "V" ++ (show id)

instance Show a => Show (Equation a) where
  show (Equation exp) = (show exp) ++ " == 0"
