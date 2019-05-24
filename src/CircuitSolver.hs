module CircuitSolver where

import Prelude
import Circuit
import Util
import Data.List
import qualified Data.Set as S
import qualified Data.Vector as V
import Numeric.LinearAlgebra

data AnalysisParameter = Parameter {
  timeInterval  :: Double,
  stepNum       :: Int
}

type State    = [(Variable, Double)]

type Solution = [(Variable, V.Vector Double)]

getConnections :: Circuit -> [[(ComponentId, LeadId)]]
getConnections circuit = connections where
  Circuit components = circuit

  getComponentConnections c = [(nodeId' c lid, lid) | lid <- (leadIds' c)]
  mapInsert y xs = map (\(x,z) -> (x,y,z)) xs

  rawConnections = concat $ zipWith mapInsert [0..] css where
    css = map getComponentConnections components
  
  classify :: NodeId -> [(NodeId, ComponentId, LeadId)] -> [[(ComponentId, LeadId)]]
  classify nid xs
    | zs == []  = [ys']
    | otherwise = ys' : (classify (nid+1) zs)
      where
      checkNid nid (x,_,_) = x == nid
      (ys, zs) = partitionWhile (checkNid nid) xs
      ys' = map (\(x,y,z) -> (y,z)) ys

  connections = classify 0 (sort rawConnections)

componentEquations :: Circuit -> [Equation]
componentEquations circuit = eqs where
  Circuit components = circuit
  eqs = concat $ zipWith equations' components [0..]

nodeEquations :: Circuit -> [Equation]
nodeEquations circuit = eqs where
  Circuit components = circuit
  connections = getConnections circuit
  
  currentSum :: [(ComponentId, LeadId)] -> Expression
  currentSum [] = Zero
  currentSum ((cid, lid):xs) = Add i (currentSum xs) where
    i = current' (components !! cid) lid cid
  
  eqs = map (Equation . currentSum) connections

getVariables :: Expression -> S.Set Variable
getVariables (Add x y) = S.union (getVariables x) (getVariables y)
getVariables (Sub x y) = S.union (getVariables x) (getVariables y)
getVariables (Mul x y) = S.union (getVariables x) (getVariables y)
getVariables (Div x y) = S.union (getVariables x) (getVariables y)
getVariables (Neg x) = getVariables x
getVariables (Inv x) = getVariables x
getVariables (Var x) = S.singleton x
getVariables (Const _)  = S.empty
getVariables Zero       = S.empty

diff :: Expression -> Variable -> Expression
diff (Add e1 e2) x = Add (diff e1 x) (diff e2 x)
diff (Sub e1 e2) x = Sub (diff e1 x) (diff e2 x)
diff (Mul e1 e2) x = Add (Mul (diff e1 x) e2) (Mul e1 (diff e2 x))
diff (Div e1 e2) x = diff (Mul e1 (Inv e2)) x
diff (Neg e) x = Neg (diff e x)
diff (Inv e) x = Neg (Div (diff e x) (Mul e e))
diff (Var v) x  | v == x    = Const 1
                | otherwise = Zero
diff (Const _) _ = Zero
diff Zero      _ = Zero

gradient :: Expression -> [Variable] -> [Expression]
gradient e xs = map (diff e) xs

isDerivative :: Variable -> Bool
isDerivative (Derivative _) = True
isDerivative _ = False

solveState :: [Equation] -> [Variable] -> State -> Maybe State
solveState eqs vars state = Nothing

evaluate :: State -> Expression-> Double
evaluate = undefined

evaluateSolution :: Solution -> Expression -> V.Vector Double
evaluateSolution = undefined

simulate :: Circuit -> State -> AnalysisParameter -> Maybe Solution
simulate circuit init param = Nothing where
  ceqs = componentEquations circuit
  neqs = nodeEquations circuit

  eqs = ceqs ++ neqs

  vars = S.toList varset where
    varsets = map (getVariables . expression) eqs
    varset  = foldr S.union S.empty varsets

  (yvars, xvars) = partition isDerivative vars

