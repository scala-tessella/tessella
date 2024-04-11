# Conversion to SVG

Each `Tiling` object has a `toSVG(...)` method that returns a
[Scalable Vector Graphics](https://en.wikipedia.org/wiki/Scalable_Vector_Graphics) XML,
describing the vector image.

```raw
<div style="width: 600px;">
```
| ![minimal](svg/minimal.svg)                                   |
|---------------------------------------------------------------|
| _Finite set of a **[2x(3⁶);(3⁴.6)]** pattern with edges only_ |
```raw
</div>
```

## Options

Several parameters can be mixed to add one or more information layers.

### Node labels

```raw
<div style="width: 600px;">
```
| ![labels](svg/labels.svg)                                      |
|----------------------------------------------------------------|
| _Finite set of a **[2x(3⁶);(3⁴.6)]** pattern with node labels_ |
```raw
</div>
```

### Animated node order

```raw
<div style="width: 600px;">
```
| ![gonality](svg/nodeOrder.svg)                                                    |
|-----------------------------------------------------------------------------------|
| _Finite set of a **[2x(3⁶);(3⁴.6)]** pattern with animation following node order_ |
```raw
</div>
```

### Perimeter

```raw
<div style="width: 600px;">
```
| ![perimeter](svg/perimeter.svg)                                          |
|--------------------------------------------------------------------------|
| _Finite set of a **[2x(3⁶);(3⁴.6)]** pattern with highlighted perimeter_ |
```raw
</div>
```

### Polygons

```raw
<div style="width: 600px;">
```
| ![polygons](svg/polygons.svg)                                                                |
|----------------------------------------------------------------------------------------------|
| _Finite set of a **[2x(3⁶);(3⁴.6)]** pattern with filled polygons colored according to size_ |
```raw
</div>
```

### Gonality

```raw
<div style="width: 600px;">
```
| ![gonality](svg/gonality.svg)                                                                         |
|-------------------------------------------------------------------------------------------------------|
| _Finite set of a **[2x(3⁶);(3⁴.6)]** pattern with dots marking nodes with the same adjacent polygons_ |
```raw
</div>
```

### Uniformity

```raw
<div style="width: 600px;">
```
| ![uniformity](svg/uniformity.svg)                                                                    |
|------------------------------------------------------------------------------------------------------|
| _Finite set of a **[2x(3⁶);(3⁴.6)]** pattern with dots marking nodes with the same adjacent pattern_ |
```raw
</div>
```
