module NumericalAnalysis where

import Expression
import Data.List
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

solveRK4 :: Variable v =>
  [Equation v] -> v -> [v] -> [v] -> [v]
  -> (Double,Double) -> [Double] -> Int -> [Expression v]
  -> [V.Vector Double]
solveRK4 eqs t ys ys' zs (ti,tf) ysInit n rexps = result where
  -- length ys == length ys'
  exps  = map lhs eqs
  vars  = ys' ++ zs

  h     = (tf - ti) / (fromIntegral n :: Double)
  dim   = length ys

  partialState tVal ysVal = HM.fromList $ (t,tVal) : (zip ys ysVal)

  transition tVal ysVal = (totalState, nysVal) where
    pstate1 = partialState tVal ysVal
    exps1   = map (partialEvaluate pstate1) exps
    newton1 = solveNewton (map Equation exps1) vars
    (ys'Val1, zsVal1) = splitAt dim newton1
    k1s = map (*h) ys'Val1

    pstate2 = partialState (tVal+h/2) (zipWith (+) ysVal (map (/2) k1s))
    exps2   = map (partialEvaluate pstate2) exps
    newton2 = solveNewton (map Equation exps2) vars
    (ys'Val2, zsVal2) = splitAt dim newton2
    k2s = map (*h) ys'Val2

    pstate3 = partialState (tVal+h/2) (zipWith (+) ysVal (map (/2) k2s))
    exps3   = map (partialEvaluate pstate3) exps
    newton3 = solveNewton (map Equation exps3) vars
    (ys'Val3, zsVal3) = splitAt dim newton3
    k3s = map (*h) ys'Val3

    pstate4 = partialState (tVal+h) (zipWith (+) ysVal k3s)
    exps4   = map (partialEvaluate pstate4) exps
    newton4 = solveNewton (map Equation exps4) vars
    (ys'Val4, zsVal4) = splitAt dim newton4
    k4s = map (*h) ys'Val4

    nysVal = zipWith5 comp ysVal k1s k2s k3s k4s where
      comp = (\y k1 k2 k3 k4 -> y + k1/6 + k2/3 + k3/3 + k4/6)
    
    totalState = HM.union pstate1 (HM.fromList (zip vars newton1))

  calc wss ysVal i = do
    let tVal = ti + h*(fromIntegral i :: Double)
        (totalState, nysVal) = transition tVal ysVal
        rvals = map (evaluate totalState) rexps
    zipWithM (\ws rval -> MV.write ws i rval) wss rvals
    return nysVal
  
  result = runST $ do
    wss <- forM rexps (\_ -> MV.new (n+1))
    foldM (calc wss) ysInit [0..n]
    forM wss V.freeze
