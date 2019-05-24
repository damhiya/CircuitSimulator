{-#
  LANGUAGE
  ExistentialQuantification
#-}

module Circuit where

type NodeId       = Int
type ComponentId  = Int
type LeadId       = Int
type VariableId   = Int

class ComponentInterface a where
  leadIds   :: a -> [LeadId]
  nodeId    :: a -> LeadId -> NodeId
  current   :: a -> LeadId -> ComponentId -> Expression
  equations :: a -> ComponentId -> [Equation]


data Component = forall a. ComponentInterface a => Component a

leadIds'    (Component com) = leadIds   com
nodeId'     (Component com) = nodeId    com
current'    (Component com) = current   com
equations'  (Component com) = equations com

data Circuit = Circuit [Component]

data Variable = Time
              | NodeVoltage NodeId
              | ComponentVariable ComponentId VariableId
              | Derivative Variable
              deriving (Eq, Ord, Show)

data Expression = Add Expression Expression
                | Sub Expression Expression
                | Mul Expression Expression
                | Div Expression Expression
                | Neg Expression
                | Inv Expression
                | Var Variable
                | Const Double
                | Zero
                deriving Show

data Equation = Equation {expression :: Expression} deriving Show
