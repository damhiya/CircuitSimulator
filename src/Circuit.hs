{-#
  LANGUAGE
  ExistentialQuantification
#-}

module Circuit where

import Expression

type NodeId       = Int
type ComponentId  = Int
type LeadId       = Int
type VariableId   = Int

data CircuitVariableId  = Time
                        | NodeVoltage NodeId
                        | ComponentVariable ComponentId VariableId
                        | Derivative (Variable CircuitVariableId)
                        deriving (Eq, Ord)

type CircuitVariable = Variable CircuitVariableId

class ComponentInterface a where
  leadIds   :: a -> [LeadId]
  nodeId    :: a -> LeadId -> NodeId
  current   :: a -> LeadId -> ComponentId -> Expression CircuitVariableId
  equations :: a -> ComponentId -> [Equation CircuitVariableId]

leadIds'    (Component com) = leadIds   com
nodeId'     (Component com) = nodeId    com
current'    (Component com) = current   com
equations'  (Component com) = equations com

data Component = forall a. ComponentInterface a => Component a

data Circuit = Circuit [Component]

instance Show CircuitVariableId where
  show Time = "Time"
  show (NodeVoltage nid) = "Node " ++ (show nid)
  show (ComponentVariable cid vid) = "Comp " ++ (show cid) ++ " " ++ (show vid)
  show (Derivative var) = "Deriv " ++ (show var)