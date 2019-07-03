import Expression
import NumericalAnalysis
import Circuit
import CommonComponent
import CircuitSolver
import qualified Data.HashMap.Strict as HM
import Data.List
import qualified Data.Set as S
import qualified Data.Vector as V

-- 1    c   2   r       0
-- +----||-----/\/\/----+
-- |                    |
-- |                    |
-- +---------|:---------+--> 0V
-- 1         v          0

v = Component $ VSource   0 1 10.0    -- 10 V
c = Component $ Capacitor 1 2 5e-6    -- 5 uF
r = Component $ Resistor  2 0 5.0     -- 5 ohm
g = Component $ Ground    0

components = [v,c,r,g]

circuit = Circuit components

state = [(NodeVoltage 0, 1.0), (NodeVoltage 1, 2.0)]

ceqs = componentEquations circuit
neqs = nodeEquations circuit

isDerivative (Derivative _) = True
isDerivative _              = False

eqs = ceqs ++ neqs

vars = S.toList varset :: [CircuitVariable] where
  varsets = map (relatedVariables . lhs) eqs
  varset  = foldr S.union S.empty varsets

t   = Time
ys' = filter isDerivative vars
ys  = map (\(Derivative y) -> y) ys'

q = current' c 

result = simulateCircuit circuit (0.0,0.0005) (HM.fromList $ zip ys [0..]) 1000 (map Var ys)
resultStrs = map show $ V.toList (result !! 0)
main = mapM_ putStrLn resultStrs