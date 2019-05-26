module NumericalAnalysis where

import Expression
import qualified Data.Vector as V
import Numeric.LinearAlgebra


data AnalysisParameter = Parameter {
  timeInterval  :: Double,
  stepNum       :: Int
}

checkTolerance :: Double -> [Double] -> Bool
checkTolerance t [] = True
checkTolerance t (x:xs)
  | x<t = checkTolerance t xs
  | otherwise = False

solveNewton :: Eq a => [Equation a] -> [Variable a] -> State a -> State a
solveNewton eqs vars param = iteration (zip vars [0,0..]) where
  exps = map expression eqs
  j = jacobian vars exps
  
  iteration last
    | pass      = last
    | otherwise = iteration new where
      state = last ++ param
      
      pass = checkTolerance 1e-9 fList

      fList  = map (evaluate state) exps
      delfList = map (map (evaluate state)) j

      c = length vars

      f    = matrix 1 fList
      delf = matrix c (concat delfList)

      Just deltav = linearSolve delf (-f)

      deltavList = concat (toLists deltav)

      new = zipWith update last deltavList
      update (var,val) delta = (var, val+delta)


type RK4Solution a = [(Variable a, V.Vector Double)]

solveRK4 :: [Equation a] -> [Variable a] -> State a -> RK4Solution a
solveRK4 = undefined

evaluateFromRK4Solution :: RK4Solution a -> Expression a -> V.Vector Double
evaluateFromRK4Solution = undefined