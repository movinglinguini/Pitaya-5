{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module Pitaya (
  Pitaya.Node (..), 
  Pitaya.Path (..), 
  Pitaya.Pulp (..),
  pitaya
) where

{-
  Developer: Luis Garcia
  Description: Implements Pitaya, a small Diagrams library for generating points
  that can be used to draw neat figures.
-}

import Diagrams.Prelude
    ( translate,
      (@@),
      atan2A,
      cosA,
      sinA,
      turn,
      p2,
      (#),
      (^.),
      (*^),
      Angle,
      HasTheta(_theta),
      HasR(_r),
      P2,
      Metric(distance),
      R1(_x),
      R2(_y) )  
import Diagrams.TwoD.Vector ( e )

{-
  A node gives suggestions for how to construct
  concrete nodes.
-}
data Node = Node {
  nRadius :: Double
  , nTurns :: Double
}

{-
  A path glues nodes together, and contains
  instructions on how to place the nodes in
  2D space.
-}
data Path = Edge {
  edgeid :: String
  , node :: Pitaya.Node
  , eTurns :: Double
  , eRadius :: Double
  , next :: Pitaya.Path
} | Null

{- 
  Paths form monoids in pitaya. Null acts as the additive unit,
  and appendPath is the binary operator.
-}
appendPath :: Pitaya.Path -> Pitaya.Path -> Pitaya.Path
appendPath Null Null = Null
appendPath p1 Null = p1
appendPath Null p2 = p2
appendPath (Edge { .. }) p2 = Edge { 
  edgeid = edgeid, 
  node = node, 
  eTurns = eTurns, 
  eRadius = eRadius, 
  next = appendPath next p2 
} 

instance Semigroup Pitaya.Path where
  (<>) = appendPath

instance Monoid Pitaya.Path where
  mempty = Null

data Pulp = Pulp {
  point :: (P2 Double)
  , fromPath :: Pitaya.Path
}

{-
  A concrete node gives instructions for how to
  generate the points that are output by Pitaya.
-}
data Seed = Seed {
  pos :: (Double , Angle Double)
  , radiusStart :: Double
  , radiusEnd :: Double
  , turnStart :: Double
  , turnEnd :: Double
  , cFromPath :: Pitaya.Path
}

{-
  Utility functions
-}

-- Given polar coordinates, create a point in Euclidean space
fromPolar :: (Double, Angle Double) -> P2 Double
fromPolar (r, theta) = p2 (r * cosA theta, r * sinA theta)

-- Linear interpolation between a start value and a stop value.
lerp :: Double -> Double -> Double -> Double
lerp s1 s2 int = s1 + (int * (s2 - s1))

{-
  Core functions
-}
-- Generate points around concrete nodes
generatePulp :: [Seed] -> [Pulp]
generatePulp [] = []
generatePulp (n : ns) = foldr 
  (\ n' -> (++) ( 
    [ 
      Pulp {
        point = fromPolar (Pitaya.lerp (radiusStart n') (radiusEnd n') int , Pitaya.lerp (turnStart n') (turnEnd n') int @@ turn) 
          # translate (fst (pos n') *^ e (snd (pos n')))
        , fromPath = cFromPath n'
      }
      | int <- [0, 0.01 .. 1] 
    ]
  ))
  []
  (n : ns)

{-
  Walk along the path and generate concrete nodes. This is where much of the complexity lies because
  the placement of each concrete node depends on the placement of the last concrete node.
-}
generateSeeds :: Pitaya.Path -> Pitaya.Path -> P2 Double -> Double -> Double -> [Seed]
generateSeeds Null _ _ _ _ = []
generateSeeds edge lastEdge lastCenter lastRadiusEnd lastTurnEnd 
  = seed : generateSeeds (next edge) edge nextCenter nextRadiusEnd nextTurnEnd
  where
    -- Place the next node by standing at the last node, turning and then walking forward
    -- the prescribed amount. 
    nextCenter :: P2 Double
    nextCenter = lastCenter # translate (eRadius lastEdge *^ e (eTurns lastEdge @@ turn))

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
    nextRadiusEnd = nRadius (node edge)

    nextTurnEnd :: Double
    nextTurnEnd = nextThetaStart ^. turn + nTurns (node edge)

    seed :: Seed
    seed = Seed {
      pos = (nextCenter ^. _r , nextCenter ^. _theta)
      , radiusStart = nextRadiusStart
      , radiusEnd = nextRadiusEnd
      , turnStart = nextThetaStart ^. turn
      , turnEnd = nextTurnEnd
      , cFromPath = edge
    }

-- Generate pulp given a path
pitaya :: Pitaya.Path -> [Pulp]
pitaya Null = mempty
pitaya p = generatePulp $ generateSeeds p startEdge startPos startRad startTheta
  where
    startPos :: P2 Double
    startPos = fromPolar (0 , 1 @@ turn)

    startRad :: Double
    startRad = nRadius (node p)

    startTheta :: Double
    startTheta = 0
    -- We "prepend" the path with an empty path
    startEdge :: Pitaya.Path
    startEdge = Edge {
      edgeid = ""
      , node = Node {
        nRadius = 0
        , nTurns = 0
      }
      , eTurns = 0
      , eRadius = 0
      , next = Null
    }


