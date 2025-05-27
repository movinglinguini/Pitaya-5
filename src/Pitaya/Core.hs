{-
  Developer: Luis Garcia
  Description: Implements Pitaya, a small Diagrams library for generating points
  that can be used to draw neat figures.
-}

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Vector

module Pitaya.Core (
  Node, 
  Path, 
  pitaya
) where

{-
  A node gives suggestions for how to construct
  concrete nodes.
-}
data Node = Node {
  radius :: Double
  , turns :: Double
}

{-
  A path glues nodes together, and contains
  instructions on how to place the nodes in
  2D space.
-}
data Path = Edge {
  node :: Main.Node
  , turns :: Double
  , radius :: Double
  , next :: Main.Path
} | Null

{-
  A concrete node gives instructions for how to
  generate the points that are output by Pitaya.
-}
data ConcreteNode = CNode {
  pos :: (Double , Angle Double)
  , radiusStart :: Double
  , radiusEnd :: Double
  , turnStart :: Double
  , turnEnd :: Double
  , fromPath :: Path
}

{-
  Utility functions
-}

-- Given polar coordinates, create a point in Euclidean space
fromPolar :: (Double, Angle Double) -> P2 Double
fromPolar (r, theta) = p2 (r * cosA theta, r * sinA theta)

-- Linear interpolation between a start value and a stop value.
lerp :: Double -> Double -> Double -> Double
lerp start stop int = start + (int * (stop - start))

{-
  Core functions
-}
-- Generate points around concrete nodes
generatePoints :: [ConcreteNode] -> [P2 Double]
generatePoints [] = []
generatePoints (n : ns) = foldr 
  (\ n -> (++) ( 
    [ 
      fromPolar (Main.lerp (radStart n) (radEnd n) int , Main.lerp (thetaStart n) (thetaEnd n) int @@ turn) 
      # translate (fst (pos n) *^ e (snd (pos n)))
      | int <- [0, 0.01 .. 1] 
    ]
  ))
  []
  (n : ns)

{-
  Walk along the path and generate concrete nodes. This is where much of the complexity lies because
  the placement of each concrete node depends on the placement of the last concrete node.
-}
generateConcreteNodes :: Main.Path -> Main.Path -> P2 Double -> Double -> Double -> [ConcreteNode]
generateConcreteNodes Null _ _ _ _ = []
generateConcreteNodes edge lastEdge lastCenter lastRadiusEnd lastTurnEnd 
  = cNode : generateConcreteNodes (next edge) edge nextCenter nextRadiusEnd nextTurnEnd
  where
    -- Place the next node by standing at the last node, turning and then walking forward
    -- the prescribed amount. 
    nextCenter :: P2 Double
    nextCenter = lastCenter # translate (radius lastEdge *^ e (turns lastEdge @@ turn))

    -- The starting radius of the next concrete node is the distance of the target radius of the last
    -- concrete node
    nextRadiusStart :: Double
    nextRadiusStart = distance nextCenter (lastCenter # translate (lastRadiusEnd *^ e (lastTurnEnd @@ turn)))

    x' :: Double
    x' = (lastCenter # translate (lastRadiusEnd *^ e (lastTurnEnd @@ turn))) ^. _x - nextCenter ^. _x
    y' :: Double
    y' = (lastCenter # translate (lastRadiusEnd *^ e (lastTurnEnd @@ turn))) ^. _y - nextCenter ^. _y

    nextThetaStart :: Angle Double
    nextThetaStart = atan2A y' x'

    nextRadiusEnd :: Double
    nextRadiusEnd = radius (node edge)

    nextTurnEnd :: Double
    nextTurnEnd = nextThetaStart ^. turn + turns (node edge)

    cNode :: ConcreteNode
    cNode = CNode {
      pos = (nextCenter ^. _r , nextCenter ^. _theta)
      , radiusStart = nextRadiusStart
      , radiusEnd = nextRadiusEnd
      , turnStart = nextThetaStart ^. turn
      , turnEnd = nextTurnEnd
      , fromPath = edge
    }

-- Generate points given a path
pitaya :: Main.Path -> [P2 Double]
pitaya Null = mempty
pitaya p = generatePoints $ generateConcreteNodes p startEdge startPos startRad startTheta
  where
    startPos :: P2 Double
    startPos = fromPolar (0 , 1 @@ turn)

    startRad :: Double
    startRad = radius (node p)

    startTheta :: Double
    startTheta = 0
    -- We "prepend" the path with an empty path
    startEdge :: Main.Path
    startEdge = Edge {
      node = Node {
        radius = 0
        , turns = 0
      }
      , edgeAng = 0
      , edgeRad = 0
      , next = Null
    }
