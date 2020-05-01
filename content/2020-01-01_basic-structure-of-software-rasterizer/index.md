+++
title = "Basic strcture of a ECMAScript 6 software rasterizer"
draft = true

[taxonomies]
tags = ["JavaScript", "rasterization"]
+++

As described in the [last post](/) we need data-format to hold a set of shapes
with certain properties.

As the shapes also overlap each other we will use the good old Array and
use the index of the shapes to convey in which order they are
stacked upon each other (this is also called the z-index).
We will simply lay the shapes in the same order on our canvas as they
appear in the Array. So the first element in the Array is the lowest shape
(with the lowest z-index) and the last element is the uppermost shape
(with the highest z-index).

```javascript

let listOfShapes = [
  {
    type: 'circle',
    radius: 2,
    center: {
      x: 5,
      y: 5
    },
    color: 'red'
  },
  {
    type: 'rectangle',
    width: 4,
    height: 2,
    origin: {
      x: 1,
      y: 1
    },
    color: 'green'
  }
]
```


The output data-format needs to hold a set of pixels where each pixel has a
specific x and y position and a specific color.
Therefore we will use a list of rows where each row is defined as a list of
pixels and each pixel is simply a hexadecimal number defining the color.

```javascript

let imageRows = [
  [
    #00ff00,
    #00ff00,
    #00ff00,
    #00ff00
  ],
  [
    #ff0000,
    #ff0000,
    #ff0000,
    #ff0000
  ],
  [
    #000000,
    #555555,
    #aaaaaa,
    #ffffff
  ]
]
```

In comparison to a simple set or list of pixels this has the advantage that we
can access a pixel at position `x = 2` and `y = 3` simply by calling
`imageRows[3][2]` or generally speaking `imageRows[y][x]`.
