module CircuitSolver where

import Prelude
import Circuit
import Expression
import Data.List
import Data.Maybe
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as S
import qualified Data.Vector as V
import NumericalAnalysis

getConnections :: Circuit -> [[(ComponentId, LeadId)]]
getConnections circuit = connections where
  Circuit components = circuit

  getComponentConnections c = [(nodeId' c lid, lid) | lid <- (leadIds' c)]
  mapInsert y xs = map (\(x,z) -> (x,y,z)) xs

  rawConnections = concat $ zipWith mapInsert [0..] css where
    css = map getComponentConnections components
  
  partitionWhile :: (a -> Bool) -> [a] -> ([a],[a])
  partitionWhile f [] = ([],[])
  partitionWhile f (x:xs)
    | f x       = (x:as,bs)
    | otherwise = ([],x:xs)
      where
      (as,bs) = partitionWhile f xs

  classify :: NodeId -> [(NodeId, ComponentId, LeadId)] -> [[(ComponentId, LeadId)]]
  classify nid xs
    | zs == []  = [ys']
    | otherwise = ys' : (classify (nid+1) zs)
      where
      checkNid nid (x,_,_) = x == nid
      (ys, zs) = partitionWhile (checkNid nid) xs
      ys' = map (\(x,y,z) -> (y,z)) ys
  
  connections = classify 0 (sort rawConnections)

componentEquations :: Circuit -> [Equation CircuitVariable]
componentEquations circuit = eqs where
  Circuit components = circuit
  eqs = concat $ zipWith equations' components [0..]

nodeEquations :: Circuit -> [Equation CircuitVariable]
nodeEquations circuit = eqs where
  Circuit components = circuit
  connections = getConnections circuit
  
  currentSum :: [(ComponentId, LeadId)] -> Expression CircuitVariable
  currentSum [] = Zero
  currentSum ((cid, lid):xs) = Add i (currentSum xs) where
    i = current' (components !! cid) lid cid
  
  eqs = map (Equation . currentSum) connections

simulateCircuit :: Circuit
                -> (Double,Double)
                -> State CircuitVariable
                -> Int
                -> [Expression CircuitVariable]
                -> [V.Vector Double]
simulateCircuit circuit (ti,tf) init n rexps = result where
  ceqs = componentEquations circuit
  neqs = nodeEquations circuit

  eqs = ceqs ++ neqs

  vars = S.toList varset where
    varsets = map (relatedVariables . lhs) eqs
    varset  = foldr S.union S.empty varsets
  
  t   = Time
  ys' = filter isDerivative vars where
    isDerivative (Derivative _) = True
    isDerivative _              = False

  ys  = map (\(Derivative y) -> y) ys'
  zs  = filter (\z -> (not . elem z) others) vars where
    others = t : ys ++ ys'
  
  ysInit = map (\y -> fromJust $ HM.lookup y init) ys

  result = solveRK4 eqs t ys ys' zs (ti,tf) ysInit n rexps