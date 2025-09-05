# Pitaya API Documentation

**Developer:** Luis Garcia  
**Description:** Pitaya is a small Diagrams library for generating points that can be used to draw neat figures.

## Module Overview

The `Pitaya` module provides a functional approach to creating and manipulating geometric paths through nodes and edges, with utilities for generating concrete points for drawing.

## Data Types

### Node
A node provides suggestions for constructing concrete nodes.

```haskell
data Node = Node {
  nRadius :: Double    -- Radius of the node
  , nTurns :: Double   -- Number of turns for the node
}
```

### Path
A path glues nodes together and contains instructions on how to place nodes in 2D space.

```haskell
data Path = Edge {
  edgeid :: String        -- Unique identifier for the edge
  , node :: Pitaya.Node   -- The node associated with this edge
  , eTurns :: Double      -- Edge turns
  , eRadius :: Double     -- Edge radius
  , next :: Pitaya.Path   -- Next edge in the path
} | Null                  -- Empty path terminator
```

### Pulp
Represents a generated point with its associated path information.

```haskell
data Pulp = Pulp {
  point :: P2 Double      -- 2D point coordinates
  , fromPath :: Pitaya.Path -- Source path that generated this point
}
```

### Seed (Internal)
A concrete node that provides instructions for generating points.

```haskell
data Seed = Seed {
  pos :: (Double, Angle Double)  -- Position in polar coordinates
  , radiusStart :: Double        -- Starting radius
  , radiusEnd :: Double          -- Ending radius
  , turnStart :: Double          -- Starting turn angle
  , turnEnd :: Double            -- Ending turn angle
  , cFromPath :: Pitaya.Path     -- Source path
}
```

## Core Functions

### Path Construction

#### `empte :: Pitaya.Path`
Creates an empty edge that serves as a starting point for building more complex paths.

```haskell
empte = Edge {
  edgeid = ""
  , node = Node { nRadius = 0, nTurns = 0 }
  , eTurns = 0
  , eRadius = 0
  , next = Null
}
```

#### `eId :: String -> Pitaya.Path -> Pitaya.Path`
Sets the identifier for an edge.

**Parameters:**
- `id`: String identifier
- `p`: Path to modify

**Returns:** Modified path with new identifier

#### `eTrns :: Double -> Pitaya.Path -> Pitaya.Path`
Sets the edge turns for an edge.

**Parameters:**
- `t`: Number of turns
- `p`: Path to modify

**Returns:** Modified path with new turn value

#### `eRds :: Double -> Pitaya.Path -> Pitaya.Path`
Sets the radius for an edge.

**Parameters:**
- `r`: Radius value
- `p`: Path to modify

**Returns:** Modified path with new radius

#### `eNd :: Node -> Pitaya.Path -> Pitaya.Path`
Sets the node for an edge.

**Parameters:**
- `n`: Node to set
- `p`: Path to modify

**Returns:** Modified path with new node

#### `eNxt :: Pitaya.Path -> Pitaya.Path -> Pitaya.Path`
Sets the next edge in a path.

**Parameters:**
- `np`: Next path to link
- `p`: Current path

**Returns:** Modified path with new next edge

### Path Manipulation

#### `rvrs :: Node -> Node`
Reverses the drawing direction of a node by negating its turns.

**Parameters:**
- `n`: Node to reverse

**Returns:** Node with reversed direction

#### `pathToList :: Pitaya.Path -> [Pitaya.Path]`
Segments a path into a list of edges while maintaining connectivity.

**Parameters:**
- `p`: Path to segment

**Returns:** List of connected edges

#### `appendPath :: Pitaya.Path -> Pitaya.Path -> Pitaya.Path`
Appends two paths together.

**Parameters:**
- `p1`: First path
- `p2`: Second path

**Returns:** Combined path

#### `pathMap :: (Pitaya.Path -> Pitaya.Path) -> Pitaya.Path -> Pitaya.Path`
Maps a function over a path, similar to `map` for lists.

**Parameters:**
- `f`: Function to apply to each edge
- `p`: Path to map over

**Returns:** New path with function applied to each edge

### Type Class Instances

#### `Semigroup Pitaya.Path`
Paths form a semigroup with `appendPath` as the binary operation.

#### `Monoid Pitaya.Path`
Paths form a monoid with `Null` as the identity element.

### Point Generation

#### `pitaya :: Pitaya.Path -> [Pulp]`
Main function that generates pulp (points) from a path.

**Parameters:**
- `p`: Path to generate points from

**Returns:** List of generated pulp

**Example:**
```haskell
-- Create a simple path and generate pulp that forms a circle
let myPath = empte # eId "path1" # eRds 10 # eTrns 0.5 # eNd (Node 5 1.0)
let points = pitaya myPath
```

## Utility Functions

### `fromPolar :: (Double, Angle Double) -> P2 Double`
Converts polar coordinates to Euclidean 2D points.

**Parameters:**
- `(r, theta)`: Polar coordinates (radius, angle)

**Returns:** 2D point in Euclidean space

### `lerp :: Double -> Double -> Double -> Double`
Linear interpolation between start and stop values.

**Parameters:**
- `s1`: Start value
- `s2`: Stop value
- `int`: Interpolation factor (0 to 1)

**Returns:** Interpolated value

## Internal Functions

### `generatePulp :: [Seed] -> [Pulp]`
Generates pulp points from a list of seeds.

### `generateSeeds :: Pitaya.Path -> Pitaya.Path -> P2 Double -> Double -> Double -> [Seed]`
Walks along a path and generates concrete nodes (seeds).

## Usage Examples

### Basic Path Creation
```haskell
-- Create a simple path
let simplePath = empte 
  # eId "myPath" 
  # eRds 10 
  # eTrns 0.5 
  # eNd (Node 5 1.0)

-- Generate pulp from the path
let points = pitaya simplePath
```

### Complex Path Construction
```haskell
-- Create a more complex path with multiple edges
let complexPath = empte 
  # eId "edge1" 
  # eRds 10 
  # eTrns 0.5 
  # eNd (Node 5 1.0)
  # eNxt (empte 
    # eId "edge2" 
    # eRds 15 
    # eTrns -0.3 
    # eNd (Node 8 0.5))

-- Generate pulp
let points = pitaya complexPath
```

### Path Manipulation
```haskell
-- Reverse a node
let reversedNode = rvrs (Node 5 1.0)

-- Map a function over a path
let modifiedPath = pathMap (\e -> e # eRds 20) myPath

-- Convert path to list
let edgeList = pathToList myPath
```

## Notes

- The library is designed to work with the Diagrams library
- Paths are built functionally using the provided utility functions
- The `pitaya` function is the main entry point for generating drawable points
- All path construction functions return new paths rather than modifying existing ones
- The library uses polar coordinates internally for geometric calculations
