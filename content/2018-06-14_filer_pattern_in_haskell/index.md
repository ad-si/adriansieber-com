+++
title = "Filter Pattern in Haskell"
date = 1970-01-01
draft = true

[taxonomies]
tags = ["Haskell"]
+++

adius [6:14 PM]
Short question:

```data
Color = Red | Green | Blue | AnyColor
getCarsWithColor :: Color -> [Car]
-- vs
data Color = Red | Green | Blue
getCarsWithColor :: Maybe Color -> [Car]```
(edited)
Both versions don't feel quite right. Is there a better pattern?

roelof [6:17 PM]
@adius I think its depends on if there are cars without a color in your oponion
but why does both versions feel not right. With my little knowlegde they look good to me

adius [6:18 PM]
There  aren't. That's the Problem :sweat_smile:

roelof [6:18 PM]
then I would use the first one

lyxia [6:18 PM]
```data Color = Red | Green | Blue
getCarsWithColor :: Color -> [Car]
getAllCars :: [Car]```

adius [6:18 PM]
If you see `getCarsWithColor Nothing` without context it's way worse than `getCarsWithColor AnyColor` (edited)
However, adding a `AnyColor` value feels quite dangerous and incorrect. E.g. `paintCarIn AnyColor` would make no sense.
@lyxia That's basically what I do right now ...

Christopher Davenport [6:21 PM]
`getCarsWithColor :: Color -> [Car] -> [Car]` seems better

roelof [6:22 PM]
@Christopher Davenport oke, your idea is to filter out the cars out of a list of cars, Right  ?

adius [6:23 PM]
I think I once even created a new data type like `data Filter a  =  Any | Only a`
Mh ... seeing it written down again, I think that's probably still the best version :smile:

roelof [6:25 PM]
then you must do it

adius [6:26 PM]
I'd love to hear @bitemyapp's opinion on this one. He always knows good patterns for such little things...

bitemyapp [6:27 PM]
@Christopher Davenport's answer looks like what I'd do without knowing more.

adius [6:28 PM]
:stuck_out_tongue: ... but I query the cars from a database so I never have the full list and need to know beforehand if It should be a full query or only for a subselection
```
