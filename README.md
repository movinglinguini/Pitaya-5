# Pitaya

## An EDSL for drawing using polar coordinates.

### API
See `API.md`.

### Prerequisites
You'll need the [Diagrams](https://diagrams.github.io/) library installed. At least follow the [quick start tutorial](https://diagrams.github.io/doc/quickstart.html) for a run down on how to draw SVGs using Diagrams, at least the section titled "Your first diagram".

### Tutorial
#### Drawing with compasses
In Pitaya, we draw particular kinds of drawings. Think about drawing with a [compass](https://en.wikipedia.org/wiki/Compass_(drawing_tool)). By keeping the needle stationary, you can draw an arc, a circle or a spiral centered around it. Now, suppose you draw an arc, then, keeping the pencil tip stationary, move the needle to a point on a circle centered around the pencil tip. Then you can place the needle down and continue drawing from there. This is what drawing with Pitaya is like.

#### Nodes and Paths
To draw with Pitaya, we use `Nodes` chained together in `Paths`. The `Nodes` are roughly the circular forms we can draw with the compass. The `Paths` specify where nodes are placed relative to each other. Below are the types for `Node` and `Path`.

```haskell
data Node = Node {
  nRadius :: Double
  , nTurns :: Double
}

data Path = Edge {
  edgeid :: String
  , node :: Pitaya.Node
  , eTurns :: Double
  , eRadius :: Double
  , next :: Pitaya.Path
} | Null
```

A `Node` has two components: a radius (`nRadius`) and the amount of turns to draw around its center (`nTurns`). So, for an example, we can make a `Node` that is supposed to be just a circular with a radius of 10 pixels like so:

```haskell
node :: Node
node = Node {
  nRadius = 10
  , nTurns = 1
}
```

If we want to _draw_ this node, Pitaya requires that it's part of a `Path`. Rather than create a path using the `Path` type constructor, let's use some shorthand to create a singleton path with our node.

```haskell
path :: Path
path = empte # eNd node
```

`empte` is an empty edge, and `eNd` takes the empty path and returns one where the node is `node`. Thus, we have a path with a single edge, and that edge carries just our node.


#### Drawing with Paths
Now, we need to do two things: 
1. produce the points that we're going to actually draw on the screen, and 
2. tell our program how to actually render those points.

For part 1, we simply call `pitaya` on the path we created above. Literally, we just need `pitaya path`. This computes to a list of points, which we dub `Pulp`.

For part 2, we want a function that uses the pulp to decide where and how to draw on the screen. I typically just draw circles centered at the points contained in the pulp, and use the identifier of the `Path` that the pulp comes from to decide the style of the circle. So, something like this:

```haskell
-- Render points so that points coming from edges labeled "edgeid" are circles
-- with radius 0.5, or radius 1 otherwise.
render :: Pulp -> Diagram B
render p
  | edgeid (fromPath p) == "edgeid" = circle 0.5
  | otherwise = circle 1
```

We now know (mostly) everything we need to know to draw our very first Pitaya drawing! Reading the code below, you might notice that the function `drawAtPoints` has come out of nowhere. This is a function that processes the points from the pulp into containers for Diagram's `Diagram B` values. Effectively, it runs `render` at a location in 2D space. If you want to know more, definitely check out the Diagrams tutorials.

```haskell
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
render p = circle 0.5

-- Draw a diagram at each point designated by the pulp
drawAtPoints :: (Pulp -> Diagram B) -> [Pulp] -> Diagram B
drawAtPoints _ [] = mempty
drawAtPoints f (p : ps) = drawAtPoints f ps <> atPoints [ point p ] (repeat $ f p)

-- Render the points on the circle
drawing :: Diagram B
drawing = (drawAtPoints render $ pitaya circlePath)

-- Output the drawing as SVG
main = mainWith drawing
```

Your output should be just a circle with circles for points:

<img src="./tutorial-figures/circle/circle.svg">

#### Composing paths
If you looked at `API.md`, you may have seen that the `Path` datatype implements the `Monoid` typeclass. The takeaway for this is that we can combine paths by using the operator `<>`. We also have a unit for the operator, `mempty`.

So, let's use this to complicate our drawing a little bit.

```haskell
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

-- Import requisite diagrams modules
import Diagrams.Prelude hiding (render)
import Diagrams.Backend.SVG.CmdLine
import Pitaya

-- circle
circlePath :: Pitaya.Path
circlePath = empte # eNd (Node 10 1) # eRds 20 # eTrns 0.25

-- spiral
spiralPath :: Pitaya.Path
spiralPath = empte # eNd (Node 10 2)

-- render the points on the circle
render :: Pulp -> Diagram B
render p = circle 0.5

-- Draw a diagram at each point designated by the pulp
drawAtPoints :: (Pulp -> Diagram B) -> [Pulp] -> Diagram B
drawAtPoints _ [] = mempty
drawAtPoints f (p : ps) = drawAtPoints f ps <> atPoints [ point p ] (repeat $ f p)

-- Render the points on the circle
drawing :: Diagram B
drawing = (drawAtPoints render $ pitaya (circlePath <> spiralPath))

-- Output the drawing as SVG
main = mainWith drawing
```

You may have noticed that the definition of `circle` is a little bit more complicated, too. What we did here was make it so that the center for the next node is 10 units away at 0.25 turns (counterclockwise). The definition for `spiral` is simple: it's just the old definition for circle. However, we won't get two circles. Instead, what should result is a circle with a spiral directly above it.

<img src="./tutorial-figures/circle-with-spiral/circle-with-sprial.svg">

### More Pitaya
See [my repository of Pitaya drawings](https://github.com/movinglinguini/pitaya-5-drawings) for more examples!