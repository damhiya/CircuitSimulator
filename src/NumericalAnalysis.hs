module NumericalAnalysis where

import Expression
import Numeric.LinearAlgebra
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Control.Monad.ST

solveNewton :: Eq a => [Equation a] -> [Variable a] -> State a
solveNewton eqs vars 
  | dime == dimv = iterate state
  | otherwise    = error "dimension mismatching"
    where
    dime = length eqs
    dimv = length vars
    dim = dime

    exps = map expression eqs
    jacobianExpression = map (map collapse) (jacobian vars exps)

    state = zip vars [0,0..]
    
    iterate lastState
      | pass      = lastState
      | otherwise = iterate new where
        fList        = map (evaluate lastState) exps
        jacobianList = map (map (evaluate lastState)) jacobianExpression

        pass = checkTolerance 1e-9 fList

        c = length vars

        f        = matrix 1 fList
        jacobian = matrix c (concat jacobianList)

        Just dvars = linearSolve jacobian (-f)

        dvarsList = concat (toLists dvars)

        new = zipWith update lastState dvarsList

        update (var,val) delta = (var, val+delta)

    checkTolerance t [] = True
    checkTolerance t (x:xs)
      | abs(x) < t     = checkTolerance t xs
      | otherwise = False
      
data RK4Parameter = Parameter {
  interval  :: (Double,Double),
  stepNum   :: Int
}

type RK4Solution a = [(Variable a, V.Vector Double)]

solveRK4 :: [Equation a] -> Variable a -> [(Variable a, Variable a)]
            -> State a -> RK4Parameter -> RK4Solution a
solveRK4 eqs time varPack init param = undefined

evaluateFromRK4Solution :: RK4Solution a -> Expression a -> V.Vector Double
evaluateFromRK4Solution = undefined