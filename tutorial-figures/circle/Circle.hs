{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

-- Import requisite diagrams modules
import Diagrams.Prelude hiding (render)
import Diagrams.Backend.SVG.CmdLine
import Pitaya

-- circle
circlePath :: Pitaya.Path
circlePath = empte # eNd (Node 10 1)

-- render the points on the circle
render :: Pulp -> Diagram B
render p = circle 0.2

-- Draw a diagram at each point designated by the pulp
drawAtPoints :: (Pulp -> Diagram B) -> [Pulp] -> Diagram B
drawAtPoints _ [] = mempty
drawAtPoints f (p : ps) = drawAtPoints f ps <> atPoints [ point p ] (repeat $ f p)

-- Render the points on the circle
drawing :: Diagram B
drawing = (drawAtPoints render $ pitaya circlePath)

-- Output the drawing as SVG
main = mainWith drawing