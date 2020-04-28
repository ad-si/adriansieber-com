+++
title = "What is a Software Rasterizer?"
draft = true
+++

In order to answer this question we first of all need to make a trivial
observation:

> A software rasterizer is a software which performs rasterization.

Yeah, I know. That was unexpected.
It is important, however, to recognize that there are also hardware rasterizers
- that is a piece of hardware which performs rasterization
(commonly a part of GPUs).

So what is rasterization then?

> Rasterization is the process of converting a vector image into a raster image.

A vector-graphic is a set of simple shapes
with certain properties like form, color and position which are laid out
on an infinitely large canvas.
As our screens aren't infinite we also need to define a viewport.
This is the rectangle which defines the part of the canvas that is
supposed to get displayed.

For example: A vector image might be composed of 2 elements:

1. A circle with this properties:
  - Radius of 2
  - Center at `x = 5` and `y = 5`
  - Red color

1. A rectangle with this properties:
  - Height of 2
  - Width of 4
  - Upper left corner positioned at `x = 1` and `y = 1`
  - Green color

Furthermore lies the rectangle on top of the circle.
The section which defines our final image (the viewport)
is the rectangular area from (0,0) to (10,10).

![Vector Image](\)


A raster image in comparison is a set of pixels (picture elements) where each
one has a specific color and a specific position in a grid.
The image size is defined by the elongation of the grid in x and y direction

For example might a raster image contain 12 pixels with 4 in each row
(x-direction) and 3 in each column (y-direction).
The first row of pixels is green, the second one is red and the pixels in
the last row are white, lightgray, gray and black.

![Raster Image](/)

Now that you know what the difference
between a vector image and a raster image is and what a software rasterizer
~~can~~ is supposed to do,
we can go to the next post where we setup the basic structure of our
software rasterizer.

[Next](/)
