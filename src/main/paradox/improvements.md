# Improvements since previous project

While the aim remains the same, **Tessella** is radically different from the previous and now deprecated project at @link:[github.com/mcallisto/tessella](https://github.com/mcallisto/tessella) { open=new }.

## Main improvements

### **Logic**

Most of the algorithms have been rewritten from scratch.
In particular tessellations are always treated recursively,
from the outmost elements to the inner ones or vice-versa.

### **Scala 3**

Taking advantage of new features introduced in Scala 3, like _extension_ and _opaque type_.

### **Expanded features**

* **Conversion to SVG**:

  cleaner @link:[Scalable Vector Graphics](https://en.wikipedia.org/wiki/Scalable_Vector_Graphics) { open=new } images,
  with @link[SMIL](https://en.wikipedia.org/wiki/Synchronized_Multimedia_Integration_Language#SMIL+SVG) { open=new } animation too.


* **Circular sequences**: 

  they are ubiquitous in tessellations
  (examples: graph paths describing a perimeter, or adjacent polygons at a full vertex),
  so the project is now depending on a separate dedicated library,
  named @link:[RingSeq](https://github.com/scala-tessella/ring-seq) { open=new },
  to deal with the most common operations.

### **New features**

* **Conversion to DOT**:

  a graph description language, see @link:[DOT](https://en.wikipedia.org/wiki/DOT_(graph_description_language)) { open=new }


* **Uniformity**:

  the vertices (nodes) of a tessellation can be now grouped by transitivity class,
  see @link:[Uniform tessellation](https://mathworld.wolfram.com/UniformTessellation.html) { open=new }

## Differences

**Tessella** no longer depends, for the time being, on the excellent
@link:[Graph for Scala](https://scala-graph.org/) { open=new } library by Peter Empen,
which is still highly recommended.