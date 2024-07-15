# Mathematical definition of tiling

Mathematically, a tessellation 𝑻 of the Euclidean plane 𝔼² is a **countable** family of **closed** sets which covers the plane **without gaps** or **overlaps**,

𝑻 = { T<sub><i>1</i></sub> , T<sub><i>2</i></sub> , … }

where T<sub><i>1</i></sub> , T<sub><i>2</i></sub> , … are known as tiles of 𝑻.
 
## Conditions required 

※1 **Countable**:  The number of tiles in the tiling can be counted. However, there can be infinitely many tiles. 

※2 **Closed**:  Each tile is enclosed by its boundary. 

※3 **Without gaps**: The union of all the sets T<sub>1</sub> , T<sub>2</sub> , … is to be the whole plane, _i.e._ { T<sub><i>1</i></sub> ∪ T<sub><i>2</i></sub> ∪ … ∪ T<sub><i>n</i></sub> } = 𝔼². 

※4 **Without overlaps**: The interior of the sets are to be pairwise disjoint, _i.e._ { int T<sub><i>i</i></sub> ∩ int T<sub><i>j</i></sub> } = ∅,  ∀ (<i>i</i>, <i>j</i>) where <i>i</i> ≠ <i>j</i>.

@@@ note

The countability condition ※1 excludes families in which every tile has zero area (such as points or line segments). This is because tiles with zero area are uncountable when they are arranged without gaps between them (when condition ※3 is satisfied).

@@@

## Additional conditions

In this project, we only consider tiles which are closed topological disks, that is, its boundary is a single simple closed curve.

@@@ note

_Single_ means a curve whose ends join up to form a “loop”. _Simple_ means that there are no crossings or branches.

@@@

Therefore:

※5 **Topological disks**: each tile T<sub><i>i</i></sub> in the tiling is a closed topological disk.

Plus:

※6 **Polygons**: each tile T<sub><i>i</i></sub> in the tiling is a polygon.

※7 **Edge-to-edge**: each pair of boundaries δT<sub><i>i</i></sub> and δT<sub><i>j</i></sub> in the tiling, ∀ (<i>i</i>, <i>j</i>) where <i>i</i> ≠ <i>j</i>, intersects along a common edge, at a vertex, or none at all.

※8 **Regular polygons**: each tile T<sub><i>i</i></sub> in the tiling is a regular polygon.

@@@ note

The combination of ※7 and ※8 means that all edges have identical (unit) length.

@@@

## Finite tessellations

A finite tessellation 𝐹 is a proper subset of 𝑻, i.e. 𝐹 ⊂ 𝑻.

