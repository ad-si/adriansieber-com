+++
title = "This crazy binary pattern is GENIUS!"
draft = true

[taxonomies]
tags = ["screenplay", "algorithm", "Gray Code", "RBC"]
+++

Narrator:
Have you ever seen this crazy binary pattern?
It was invented in 1947 by Frank Gray and is therefore called "Gray Code".

You can find this pattern in all kinds of places … If you know where to look.

- [Rotary encoders](https://en.wikipedia.org/wiki/Rotary_encoder)
- TODO


But what is it good for and why is it so genius?

Let me explain it to you with a little example:


- Small measuring errors for each new positions will add up over time
   until the position is completely off.
- Hamming distance
- should be able to resolve the rotation angle at a precision of 45°.
   This means we need to encode 8 distinct values, which corresponds to a
   3 bit resolution.
- `0` is being represented by black and `1` by white.
- Furthermore we interpret the rotation angles the
   [mathematical way](https://en.wikipedia.org/wiki/Angle_of_rotation).
- The disc in accordance to those rules looks like this:
   {% include binary-code-disc.svg %}
- we simply direct the light on the disc and cut out the black parts.
   The light can then shine through the holes
   and we can measure the pattern on the other side of the disc
   with light sensitive diodes (photodiodes).
- Gray code is a binary numeral system where two successive values
   differ in only one bit.
- natural binary code first:
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
- corresponding Gray code:

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

- The code words are the same as in binary code, but in a different order.
- The Hamming distances between each code word are:
   {% include gray-code-changes.svg %}
   As you can see the Hamming distance is consistently 1, which means
   that exactly one bit changes for each incrementation.
   And this is exactly the reason why we use gray code.
