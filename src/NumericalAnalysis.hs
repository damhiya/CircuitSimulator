module NumericalAnalysis where

import Expression
import qualified Data.HashMap.Strict as HM
import Numeric.LinearAlgebra
import Control.Monad
import Control.Monad.ST
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV


solveNewton :: Variable v => [Equation v] -> [v] -> [Double]
solveNewton eqs vars 
  | dime == dimv = iterate initVals
  | otherwise    = error "dimension mismatching"
    where
    dime = length eqs
    dimv = length vars
    dim = dime

    exps = map lhs eqs
    jacobianExpression = map (map collapse) (jacobian vars exps)

    initVals = replicate dim 0.0
    
    iterate :: [Double] -> [Double]
    iterate vals
      | pass      = vals
      | otherwise = iterate newVals where
        fList        =      map (evaluate (genState vars vals)) exps
        jacobianList = map (map (evaluate (genState vars vals))) jacobianExpression

        pass = checkTolerance 1e-9 fList

        c = length vars

        f        = matrix 1 fList
        jacobian = matrix c (concat jacobianList)

        Just dvars = linearSolve jacobian (-f)

        dvarsList = concat (toLists dvars)

        newVals = zipWith (+) vals dvarsList

    genState vars vals = HM.fromList (zip vars vals)

    checkTolerance t [] = True
    checkTolerance t (x:xs)
      | abs(x) < t     = checkTolerance t xs
      | otherwise = False
      
data RK4Parameter = RK4Parameter {
  interval  :: (Double,Double),
  stepNum   :: Int
}

type RK4Solution v = [(v, V.Vector Double)]

solveRK4 :: Variable v => [Equation v] -> [v] -> v -> [v] -> [v]
            -> [Double] -> RK4Parameter -> RK4Solution v
solveRK4 eqs vars t ys ys' ysInit (RK4Parameter (ti,tf) n) = zip ys (solve) where
  tmpVars = filter (\var -> not $ elem var ([t]++ys++ys')) vars

  solve = runST $ do
    wss <- forM ys (\_ -> MV.new (n+1))

    forM (zip wss ysInit) (\(ws,ival) -> MV.write ws 0 ival)
    -- do RK4 Method
    forM wss V.freeze

evaluateFromRK4Solution :: RK4Solution a -> Expression a -> V.Vector Double
evaluateFromRK4Solution = undefined