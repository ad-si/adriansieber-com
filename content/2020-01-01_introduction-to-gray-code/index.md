+++
title = "Introduction to Gray Code"
draft = true
+++

Imagine you would sell tower cranes.
(Those are the T shaped ones you normally see on construction sites.)
Your models are top notch: Super reliable, stable, fast, yet
affordable.
However, a few weeks ago one crane operator showed up to work totally
bleary-eyed and fell asleep while operating the crane.
And if that wasn't enough, he activated the rotation mechanism during sleep
and the working arm crashed into the next building and completely destroyed
the facade.

It's of course not your fault that this douche went rouge with your
masterpiece, but nevertheless the accusation came up
that your cranes are not safe enough.
In order to make your next generation more fault-safe you were thinking
about limiting the sector where the crane can be operated to the
construction zone.
I.e. when the construction company sets up the crane it can specify the
area in which the crane can be operated and if one tries to rotate the
crane any further it simply stops.
In order to build such a crane
you need to know the current rotation angle of the crane
at any time.

One way to achieve this is to specify the absolute rotation angle during setup
and measure the change afterwards.
This, however, is not optimal.
When the crane is restarted or it loses power due to a power outage
the steering controllers memory is erased
and the crane therefore can't "remember" the original position.
It therefore would be necessary to recalibrate it every time it's restarted.

Another problem is that the measurements will be more inaccurate
the longer the crane is operated.
Small measuring errors for each new positions will add up over time
until the position is completely off.

Therefore we're better of
encoding the rotation in an absolute manner.
As our steering controller is digital, but the number of rotation angles is
infinite, we need to define a resolution for measuring the angle.
Let's say our first version of the new crane
should be able to resolve the rotation angle at a precision of 45°.
This means we need to encode 8 distinct values, which corresponds to a
3 bit resolution.

Rotation Angle | Binary Encoding
---------------|-----------------
  0° -  45°    |      000
 45° -  90°    |      001
 90° - 135°    |      010
135° - 180°    |      011
180° - 225°    |      100
225° - 270°    |      101
270° - 315°    |      110
315° - 360°    |      111

These values can now be encoded on a disc.
Therefore we define that `0` is being represented by black
and `1` by white.
Furthermore we interpret the rotation angles the
[mathematical way](https://en.wikipedia.org/wiki/Angle_of_rotation).
The disc in accordance to those rules looks like this:

{% include binary-code-disc.svg %}

Now we only need to attach the disc to the mast of the crane and
build a sensor to read out the values.
An easy way to read out the values in a mechanical way is to use light:
Light on is `1` and light off is `0`.
But instead of building a mechanism
to turn lights on and off according to the position of the crane,
we simply direct the light on the disc and cut out the black parts.
The light can then shine through the holes
and we can measure the pattern on the other side of the disc
with light sensitive diodes (photodiodes).

Here is a schematic of the final installation:

// TODO


> Gray code is a binary numeral system where two successive values
> differ in only one bit.

This is the definition which can be found on
[Wikipedia](https://en.wikipedia.org/wiki/Gray_code).
Sounds a little cryptic, doesn't it?
In order to make sense of it,
let's have a look at the natural binary code first:

 Code  | Number
:-----:|:------:
 `000` |   0
 `001` |   1
 `010` |   2
 `011` |   3
 `100` |   4
 `101` |   5
 `110` |   6
 `111` |   7

In order to understand the motivation for Gray code it's now important
to observe how the code words change for each incrementation.
The common standard for describing changes in character strings
is the *Hamming distance*.

> The Hamming distance between two strings of equal length is the number
> of positions at which the corresponding symbols are different.

{% include binary-code-changes.svg %}

And as you can see the Hamming distance fluctuates between 1 to 3.
Now let's have a look at the corresponding Gray code:

Binary Code | Gray Code | Number
:----------:|:---------:|:------:
   `000`    |   `000`   |   0
   `001`    |   `001`   |   1
   `010`    |   `011`   |   2
   `011`    |   `010`   |   3
   `100`    |   `110`   |   4
   `101`    |   `111`   |   5
   `110`    |   `101`   |   6
   `111`    |   `100`   |   7

The code words are the same as in binary code, but in a different order.
The Hamming distances between each code word are:

{% include gray-code-changes.svg %}

As you can see the Hamming distance is consistently 1, which means
that exactly one bit changes for each incrementation.
And this is exactly the reason why we use gray code.

// TODO

{% include gray-code-disc.svg %}
