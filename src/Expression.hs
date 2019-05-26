module Expression where

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

diff :: Eq a => Variable a -> Expression a -> Expression a
diff x (Add e1 e2) = Add (diff x e1) (diff x e2)
diff x (Sub e1 e2) = Sub (diff x e1) (diff x e2)
diff x (Mul e1 e2) = Add (Mul (diff x e1) e2) (Mul e1 (diff x e2))
diff x (Div e1 e2) = diff x (Mul e1 (Inv e2))
diff x (Neg e) = Neg (diff x e)
diff x (Inv e) = Neg (Div (diff x e) (Mul e e))
diff x (Var v)  | v == x    = One
                | otherwise = Zero
diff _ (Const _) = Zero
diff _ Zero      = Zero

grad :: Eq a => [Variable a] -> Expression a -> [Expression a]
grad vars exp = map (\var -> diff var exp) vars

jacobian :: Eq a => [Variable a] -> [Expression a] -> [[Expression a]]
jacobian vars exps = map (grad vars) exps

getValue :: Eq a => State a -> Variable a -> Double
getValue [] _ = error "getValue : no matching variable exist"
getValue ((x,val):xs) var
  | x == var  = val
  | otherwise = getValue xs var

evaluate :: Eq a => State a -> Expression a -> Double
evaluate s (Add e1 e2) = (evaluate s e1) + (evaluate s e2)
evaluate s (Sub e1 e2) = (evaluate s e1) - (evaluate s e2)
evaluate s (Mul e1 e2) = (evaluate s e1) * (evaluate s e2)
evaluate s (Div e1 e2) = (evaluate s e1) / (evaluate s e2)
evaluate s (Neg e) = negate (evaluate s e)
evaluate s (Inv e) = 1.0  / (evaluate s e)
evaluate s (Var v) = getValue s v
evaluate _ (Const x)  = x
evaluate _ Zero       = 0.0

data Equation a = Equation {expression :: Expression a}

instance Show a => Show (Variable a) where
  show (Variable id) = "Var " ++ (show id)

instance Show a => Show (Equation a) where
  show (Equation exp) = (show exp) ++ " == 0"
