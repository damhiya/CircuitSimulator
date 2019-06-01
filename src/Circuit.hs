{-#
  LANGUAGE
  ExistentialQuantification
#-}

module Circuit where

import Expression
import Data.Hashable

type NodeId       = Int
type ComponentId  = Int
type LeadId       = Int
type VariableId   = Int

data CircuitVariable  = Time
                      | NodeVoltage NodeId
                      | ComponentVariable ComponentId VariableId
                      | Derivative CircuitVariable
                      deriving (Eq, Ord)

class ComponentInterface a where
  leadIds   :: a -> [LeadId]
  nodeId    :: a -> LeadId -> NodeId
  current   :: a -> LeadId -> ComponentId -> Expression CircuitVariable
  equations :: a -> ComponentId -> [Equation CircuitVariable]

data Component = forall a. ComponentInterface a => Component a

leadIds'    (Component com) = leadIds   com
nodeId'     (Component com) = nodeId    com
current'    (Component com) = current   com
equations'  (Component com) = equations com

data Circuit = Circuit [Component]

instance Show CircuitVariable where
  show Time = "T"
  show (NodeVoltage nid) = "NV " ++ (show nid)
  show (ComponentVariable cid vid) = "CV " ++ (show cid) ++ " " ++ (show vid)
  show (Derivative var) = "D " ++ (show var)

instance Hashable CircuitVariable where
  hashWithSalt i Time = hash (i, hash "Time")
  hashWithSalt i (NodeVoltage nid) = hash (i, hash "NodeVoltage", nid)
  hashWithSalt i (ComponentVariable cid vid) = hash (i, hash "ComponentVariable", cid, vid)
  hashWithSalt i (Derivative var) = hash (i, hash "Derivative", hash var)
  
instance Variable CircuitVariable