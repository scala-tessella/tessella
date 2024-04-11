# Creation of regular tessellations

Finite sets of the 3 regular tessellations
(see @link:[Wikipedia](https://en.wikipedia.org/wiki/Euclidean_tilings_by_convex_regular_polygons#Regular_tilings) { open=new })
can be created.

## Triangle only, pattern [(3⁶)]

### Fast reticulate method

`Tiling.triangleNet(4, 4)`

```raw
<div style="width: 420px;">
```
| ![tri4x4Reticulate](svg/tri4x4Reticulate.svg) |
|-----------------------------------------------|
| _Finite set of a **[(3⁶)]** pattern_         |
```raw
</div>
```

`Tiling.triangleTriangle(4)`

```raw
<div style="width: 420px;">
```
| ![triangleTriangleOfSide4](svg/triangleTriangleOfSide4.svg) |
|-------------------------------------------------------------|
| _Finite set of a **[(3⁶)]** pattern_                       |
```raw
</div>
```

### Slow growth method

`Growth.growFull(FullVertex.s("(3⁶)"), 60)` or `Tiling.triangularHex(3)`

```raw
<div style="width: 600px;">
```
| ![triHexOfSide3](svg/triHexOfSide3.svg) |
|-----------------------------------------|
| _Finite set of a **[(3⁶)]** pattern_   |
```raw
</div>
```

## Square only, pattern [(4⁴)]

### Fast reticulate method

`Tiling.squareNet(4, 4)`

```raw
<div style="width: 440px;">
```
| ![fast4](svg/sqr4x4Reticulate.svg)    |
|---------------------------------------|
| _Finite set of a **[(4⁴)]** pattern_ |
```raw
</div>
```

### Slow growth method

`Growth.growFull(FullVertex.s("(4⁴)"), 9)` or
`Tiling.squareNet(3)`

```raw
<div style="width: 360px;">
```
| ![sqr3x3Growth](svg/sqr3x3Growth.svg) |
|---------------------------------------|
| _Finite set of a **[(4⁴)]** pattern_         |
```raw
</div>
```

## Hexagon only, pattern [(6³)]

### Fast reticulate method

`Tiling.hexagonNet(4, 4)`

```raw
<div style="width: 640px;">
```
| ![hex4x4Reticulate](svg/hex4x4Reticulate.svg)    |
|--------------------------------------------------|
| _Finite set of a **[(6³)]** pattern_ |
```raw
</div>
```

`Tiling.hexTrianguloid(4)`

```raw
<div style="width: 640px;">
```
| ![hexTrianguloidOfSide4](svg/hexTrianguloidOfSide4.svg) |
|---------------------------------------------------------|
| _Finite set of a **[(6³)]** pattern_        |
```raw
</div>
```

### Slow growth method

`Growth.growFull(FullVertex.s("(6³)"), 19)` or
`Tiling.hexagonalHexoid(3)`

```raw
<div style="width: 800px;">
```
| ![hexHexOfSide3](svg/hexHexOfSide3.svg)                     |
|-------------------------------------------------------------|
| _Finite set of a **[(6³)]** pattern_ |
```raw
</div>
```
