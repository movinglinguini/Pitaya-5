# Pitaya

## An EDSL for drawing using polar coordinates.

### API


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

Now, for 