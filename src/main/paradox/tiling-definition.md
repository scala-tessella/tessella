# Mathematical definition of tiling

Mathematically, a plane tiling _`T`_ is a **countable** family of **closed** sets which covers the plane **without gaps** or **overlaps**,

_`T`_ = { T<sub><i>1</i></sub> , T<sub><i>2</i></sub> , … }

where T<sub><i>1</i></sub> , T<sub><i>2</i></sub> , … are known as tiles of _`T`_.
 
## Conditions required 

※1 **Countable**:  The number of tiles in the tiling can be counted. However, there can be infinitely many tiles. 

※2 **Closed**:  Each tile is enclosed by its boundary. 

※3 **Without gaps**: The union of all the sets T<sub>1</sub> , T<sub>2</sub> , … is to be the whole plane, _i.e._ { T<sub><i>1</i></sub> ∪ T<sub><i>2</i></sub> … ∪ T<sub><i>n</i></sub> } = whole plane. 

※4 **Without overlaps**: The interior of the sets are to be pairwise disjoint, _i.e._ { interior of T<sub><i>i</i></sub> ∩ interior of T<sub><i>j</i></sub> } = ∅, where _i_ ≠ _j_ ∀ any _i_ and _j_.

@@@ note

The countability condition ※1 excludes families in which every tile has zero area (such as points or line segments). This is because tiles with zero area are uncountable when they are arranged without gaps between them (when condition ※3 is satisfied).

@@@

## Additional conditions

In this project, we only consider tiles which are closed topological disks, that is, its boundary is a single simple closed curve. Single means a curve whose ends join up to form a “loop”. Simple means that there are no crossings or branches. Therefore:

※5 **Topological disks**: each tile T<sub><i>i</i></sub> in the tiling is a closed topological disk.

Plus:

※6 **Polygons**: each tile T<sub><i>i</i></sub> in the tiling is a polygon.

※7 **Edge-to-edge**: each pair of tiles T<sub><i>i</i></sub> and T<sub><i>j</i></sub> in the tiling, where _i_ ≠ _j_ ∀ any _i_ and _j_, intersects along a common edge, at a vertex, or none at all.

※8 **Regular polygons**: each tile T<sub><i>i</i></sub> in the tiling is a regular polygon.

※9 **Unit size**: each tile T<sub><i>i</i></sub> in the tiling is a regular polygon with edges of length 1.
