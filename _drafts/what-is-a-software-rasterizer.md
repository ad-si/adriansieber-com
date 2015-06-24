# What is a software rasterizer?

In order to answer this question we need to make a trivial
observation first of all:

> A software rasterizer is a software which performs rasterization.

Yeah, I know. That was unexpected.
It is important, however, to recognize that there are also hardware rasterizers
- that is a piece of hardware which performs rasterization.

So what is rasterization then?

> Rasterization is the process of converting a vector image into a raster image.

A vector-graphic is a set of simple shapes
with certain properties like form, color and position which are positioned
on an infinitely large canvas.
Furthermore it is defined by one shape which marks the outline of
the vector-image.

For example might a vector image be composed from a circle with the radius 2,
the center x=5 and y=5 and the color red
and a rectangle with a height of 2, a width of 4,
the upper left corner positioned at x=1 and y=1 and the color green
while the rectangle lies on top of the circle.
The section which defines our image is a rectangle from (0,0) to (10,10).

![Vector Image](\)


A raster image in comparison is a set of pixels (picture elements) where each
one has a specific color and a specific position in a grid.
The image size is defined by the elongation of the grid in x and y direction

For example might a raster image contain 12 pixels with 4 in x direction
and 3 in y direction.
The first row of pixels is green, the second one is red and the pixels in
the last row are white, lightgray, gray and black.

![Raster Image](/)

Now that you know what the difference
between a vector image and a raster image is
we can go to the next post where we setup the basic structure of our
software rasterizer.

[Next](/)
