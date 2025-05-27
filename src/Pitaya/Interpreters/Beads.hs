{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

import Diagrams.Prelude
import Diagrams.TwoD.Vector
import Data.Maybe

module Pitaya.Interpreters.Beads (pitayaGen) where

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : xs) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (x : xs) = Just xs

maybeTail :: Maybe [a] -> Maybe [a]
maybeTail Nothing = Nothing
maybeTail (Just xs) = safeTail xs

maybeHead :: Maybe [a] -> Maybe a
maybeHead Nothing = Nothing
maybeHead (Just xs) = safeHead xs

maybeFromVertices :: (Maybe (P2 Double), Maybe (P2 Double)) -> Diagram B
maybeFromVertices (Nothing, _) = mempty
maybeFromVertices (_, Nothing) = mempty
maybeFromVertices (Just v1, Just v2) = fromVertices [v1, v2]

maybeAtPoints :: [Maybe (P2 Double)] -> [Diagram B] -> Diagram B
maybeAtPoints [] _ = mempty
maybeAtPoints (Nothing : ps) _ = mempty
maybeAtPoints ((Just p) : ps) os = atPoints [p] os <> maybeAtPoints ps os

draw :: Maybe [P2 Double] -> Diagram B
draw Nothing = mempty
draw (Just coords) = maybeFromVertices (c1, c2) 
  `atop` maybeAtPoints [c2] (repeat (circle 0.01 # fc blue # lw none))
  `atop` pitayaGen (safeTail coords)
  where
    c1 :: Maybe (P2 Double)
    c1 = safeHead coords
    c2 :: Maybe (P2 Double)
    c2 = maybeHead $ safeTail coords