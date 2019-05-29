module CommonComponent where

import Circuit
import Expression

data Wire = Wire NodeId NodeId

data Ground  = Ground NodeId

data VSource = VSource NodeId NodeId Double
data ISource = ISource NodeId NodeId Double

data Resistor  = Resistor  NodeId NodeId Double
data Capacitor = Capacitor NodeId NodeId Double
data Inductor  = Inductor  NodeId NodeId Double

instance ComponentInterface Wire where
  leadIds _ = [0,1]
  
  nodeId (Wire n0 _ ) 0 = n0
  nodeId (Wire _  n1) 1 = n1

  current _   0 cid = Var (ComponentVariable cid 0)
  current com 1 cid = Neg (current com 1 cid)

  equations (Wire n0 n1) cid = [eq] where
    v0 = Var (NodeVoltage n0)
    v1 = Var (NodeVoltage n1)

    eq = Equation (Sub v1 v0)

instance ComponentInterface Ground where
  leadIds _ = [0]

  nodeId (Ground n) 0 = n

  current _ 0 cid = Var (ComponentVariable cid 0)

  equations (Ground n) cid = [eq] where
    v = Var (NodeVoltage n)
    eq = Equation v

instance ComponentInterface VSource where
  leadIds _ = [0,1]

  nodeId (VSource n0 _  _) 0 = n0
  nodeId (VSource _  n1 _) 1 = n1

  current _   0 cid = Var (ComponentVariable cid 0)
  current com 1 cid = Neg (current com 0 cid)

  equations (VSource n0 n1 v) cid = [eq] where
    v0 = Var (NodeVoltage n0)
    v1 = Var (NodeVoltage n1)
    eq = Equation (Sub (Sub v1  v0) (Const v))

instance ComponentInterface ISource where
  leadIds _ = [0,1]

  nodeId (ISource n0 _  _) 0 = n0
  nodeId (ISource _  n1 _) 1 = n1

  current (ISource _ _ i) 0 _ = Const i
  current (ISource _ _ i) 1 _ = Const (-i)

  equations _ _ = []

instance ComponentInterface Resistor where
  leadIds _ = [0,1]

  nodeId (Resistor n0 _  _) 0 = n0
  nodeId (Resistor _  n1 _) 1 = n1

  current _   0 cid = Var (ComponentVariable cid 0)
  current com 1 cid = Neg (current com 0 cid)

  equations com cid = [eq] where
    Resistor n0 n1 r = com

    v0 = Var (NodeVoltage n0)
    v1 = Var (NodeVoltage n1)

    i = current com 0 cid
    v = Mul i (Const r)

    eq = Equation (Sub (Sub v0 v) v1)

instance ComponentInterface Capacitor where
  leadIds _ = [0,1]

  nodeId (Capacitor n0 _  _) 0 = n0
  nodeId (Capacitor _  n1 _) 1 = n1

  current _   0 cid = Var (Derivative (ComponentVariable cid 0))
  current com 1 cid = Neg (current com 0 cid)

  equations com cid = [eq] where
    Capacitor n0 n1 c = com

    v0 = Var (NodeVoltage n0)
    v1 = Var (NodeVoltage n1)

    q = Var (ComponentVariable cid 0)
    v = Div q (Const c)

    eq = Equation (Sub (Sub v0 v) v1)

instance ComponentInterface Inductor where
  leadIds _ = [0,1]

  nodeId (Inductor n0 _  _) 0 = n0
  nodeId (Inductor _  n1 _) 1 = n1

  current _   0 cid = Var (ComponentVariable cid 0)
  current com 1 cid = Neg (current com 0 cid)

  equations com cid = [eq] where
    Inductor n0 n1 l = com

    v0 = Var (NodeVoltage n0)
    v1 = Var (NodeVoltage n1)

    di = Var $ Derivative (ComponentVariable cid 0)
    v  = Mul di (Const l)
    
    eq = Equation (Sub (Sub v0 v) v1)