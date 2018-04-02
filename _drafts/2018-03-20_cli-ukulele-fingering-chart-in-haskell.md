---
title: <code>uku</code> - A CLI tool to display Ukulele fingering charts
---

**TLDR:**
This is a tutorial on how to write a CLI tool to display fingering
charts for Ukuleles in your terminal.
As it's written in literate Haskell
it also contains the code itself for this program.
If you just want to use the tool install it with `stack install uku`.

<!-- 2 years ago I started to write a CLI tool to display Ukulele fingering
charts in the terminal as ANSI art.
I never found the time to finish it, but now I thought it was about time.
 -->

While I originally started to write this in JavaScript 2 years ago
(I thought it's about time to finish it 😛),
I recently got introduced to Haskell and I fell in love with it.
One cool feature is that it has first class support for [literate programming].
This means this post contains all the code
and can be executed like a shell script.
([Expand documentation on how to do it.](TODO))
It also means I can explain to you how I build the tool
while writing the code for it. 😁

First a short overview of what it's actually supposed to do.
This is our target output:

![Output of command "uku g"](/img/uku-g.svg)

You specify an accord and `uku` pretty prints the fingering chart
as ANSI art [chord boxes] to the terminal.
Cool, right? So let's get started with the code:

[chord boxes]: https://www.justinguitar.com/en/BC-108-TABandBoxes.php

First some meta stuff. I'm using stack's [script interpreter]
to make it an easily executable CLI script
with no need for compilation.


<!--
```haskell
{-
stack
  --resolver lts-11.1
  script
  --package protolude
  --package cmdargs
-}
```
-->

[script interpreter]: https://docs.haskellstack.org/en/stable/GUIDE/#script-interpreter

As dependencies we only use [Neil Mitchell][ndmitchell]'s [cmdargs]
to parse CLI flags.
I'll explain the language extensions later when we need them.

[ndmitchell]: https://ndmitchell.com
[cmdargs]: https://github.com/ndmitchell/cmdargs


```haskell
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Uku where

import Data.Text as Text hiding (length)
import Data.Text.IO as Text
import Data.Map.Strict as Map
import Protolude
import System.Console.CmdArgs
import System.IO (stderr)
import Unsafe
```


Now we need some types to model our domain.
Normally you'd expect something like `data Note = C | Cis | D | Dis …` here,
but this notation is used for historical reasons
and actually makes not a lot of sense nowadays.
E.g. the distance between `E` and `F` is half the distance of `F` to `G` 🤦.
I'll call this notation the  "arachaic notation" for the rest of the post.

Actually even the notion of absolute note values isn't particularly useful,
as western music is inherently relative and therefore
you can start a song from every note.
To accomodate this we simply model everything relatively
and only make the common note names and the particluar tuning of the
Ukulele a special instance of it.
I'll call this notation the "relative notation".

A Piano supports 88 notes and MIDI supports 128 notes.
As there are 12 notes per octave we can use a base 12 ([duodecimal]) system
to simplify counting in octaves.
There are 2 special unicode characters for ten and eleven
in the duodecimal system (pitman digits): `1 2 3 4 5 6 7 8 9 ↊ ↋`.
Unfortunately they are understood as symbols in GHC (Glasgow Haskell Compiler)
and therefore not allowed in regular names.
<small>Explanation on [stackoverflow].</small>
For that reason we replace `↊` with `X`
(like the Roman literal) and `↋` with `E` (like "eleven"),
which is the recommend way by the [Dozenal Society of America][duodecimal].
Each step corresponds to one archaic notation semi tone.

[duodecimal]: http://www.dozenal.org
[stackoverflow]: https://stackoverflow.com/questions/31965349/using-emoji-in-haskell

```haskell
data Step
  = S0 | S1 | S2 | S3 | S4 | S5
  | S6 | S7 | S8 | S9 | SX | SE
```

Explicitly listing all possible distances has the advantage that we can
now ensure at compile time that no invalid intervals are specified.

```haskell
data Octave
  = O0 | O1 | O2 | O3 | O4 | O5
  | O6 | O7 | O8 | O9 | OX | OE
```

Combined, they yield our `Interval` data type.

```haskell
data Interval = Interval Octave Step
```

This amounts to 144 intervals, which is great, as it's
slightly more than the 128 notes defined in MIDI and therefore
we can model everything that MIDI can.

We can further simplify it by defining constants for
common intervals: `i1X = Interval O1 SX`,
where the first duodecimal number after the `i`
is the octave and the second one is the step.
To spare you the complete list, I append it to the end of the post.

For specifying absolute note values, which you'll need at some point,
we simply use the MIDI values in hex notation:
`data MidiNote = M00 | M01 | M02 | … | M7F | M80`.
I appended the full list to the end of the post.

We can now map this to our Ukulele:

```
Archaic notation             Relative notation (relative to E4)

A4 ╓──┬──┬──┬──┬             i07 ╓──┬──┬──┬──┬
E4 ╟──┼──┼──┼──┼             i00 ╟──┼──┼──┼──┼
C4 ╟──┼──┼──┼──┼             i04 ╟──┼──┼──┼──┼
G4 ╙──┴──┴──┴──┴             i09 ╙──┴──┴──┴──┴
```


To completely model the domain we need some more types ...
like for our fingers🤞.

```haskell
data Finger = Thumb | Index | Middle | Ring | Pinky | SomFngr
```

And now let's make it possible to pick a string at a certain position,
play the string open, or mute the string.
To model the pick position we'll have to define a fret position scale as
it's in the range 1 to _length of fretboard_
and there is no good type safe way to model this with integers or similar.
0 would mean open, but using a special value for it makes more sense.

Normally fretted instruments don't have more than around 30 frets,
so we'll just use as base36 scheme without the zero.
I.e. 1-9 and A-Z.

```haskell
data FretPosition
  =      F1 | F2 | F3 | F4 | F5 | F6 | F7 | F8 | F9 | FA | FB
  | FC | FD | FE | FF | FG | FH | FI | FJ | FK | FL | FM | FN
  | FO | FP | FQ | FR | FS | FT | FU | FV | FW | FX | FY | FZ

data Pick = Pick FretPosition Finger | Open | Mute
```

Several fingers can pick one string and that for each string.
The strings are listed from thick (low pitch) to thin (high pitch)
as that's what your fretboard looks like when you look at the ukulele
as depicted in the chord boxes.

```haskell
type Fretting = [[Pick]]
```

Finally we define a played fretted instrument by a list of the relative notes
of all strings, it's base note (the note of the lowest string)
and the current fingering / fretting pattern.

```haskell
type InstStrings = [Interval]
data PlayedInstrument = PlayedInst InstStrings MidiNote Fretting
type Instrument = Fretting -> PlayedInstrument
```

The cool thing about this representation is that if you want to change
the tuning of the instrument,
e.g. by using a [capo](https://en.wikipedia.org/wiki/Capo),
you only have to change the base note instead of changing each string.

Based on this `PlayedInstrument` data type
we can define a normal Ukulele by partially applying
the data constructor like this:

```haskell
ukulele :: Instrument
ukulele = PlayedInst [i07, i00, i04, i09, i09] M40
```

I added the type signature to make it clearer.
Our ukulele is now a function which gets applied to a finger pattern
and returns a played instrument.
Makes sense, right?

And now we can finally play our first chords on the ukulele 🎉.
For example G major:

```haskell
gMajor = ukulele [
    [Open], [Pick F2 Index], [Pick F3 Ring], [Pick F2 Middle]
  ]

--  As ANSI art:
--     G
--  ╒═╤═╤═╕
--  │_│_│_│
--  │_I_│_M
--  │_│_R_│
--  │_│_│_│
```

or B major:
(Formatted like this
to show the relation between the list format and the output.)

```haskell
bMajor = ukulele [
    [Pick F2 Index,
     Pick F4 Ring  ], [Pick F2 Index,
                       Pick F3 Middle], [Pick F2 Index], [Pick F2 Index]
  ]

--  As ANSI art:
--     G
--  ╒═╤═╤═╕
--  │_│_│_│
--  I_I_I_I
--  │_M_│_│
--  R_│_│_│
--  │_│_│_│
```

Now that we've fixed musical notation, we need a map from archaic notation
to the fingering patterns.
We could now write a function to automatically generate all of them,
but deciding which finger to use for which pick
is based on the anatomy of hands and it would be really hard to model this 😅.
So instead of overengineering it, we'll stick to a simple manually generated
lookup table.

I'll give you a short excerpt of the map,
so you know what it looks like
and move the complete one to the end of the post.
Note that for each chord there is a number of ways the chord can be picked.
(Sorted from most to least common.)

```haskell
archaicToFrettingA = Map.fromList [
    ("a", [
      [[Pick F2 Middle], [Pick F1 Index], [Open], [Open]],
      [[Pick F2 Middle], [Pick F1 Index], [Open], [Pick F4 Pinky]]
    ]),
    ("am", [
      [[Pick F2 Middle], [Open], [Open], [Open]],
      [[Pick F2 Middle], [Open], [Open], [Pick F3 SomFngr]]
    ])
  ]
```


The next step is a function which renders the fretting model in our ANSI art
chart boxes:

```haskell
chordToPlayedInst :: Text -> Instrument -> Either Text PlayedInstrument
chordToPlayedInst chord instrument =
  let
    maybeInst = do
      fretting <- Map.lookup chord archaicToFretting
      return $ instrument $ unsafeHead fretting -- TODO: Use safe head
    errorMessage = "There is no fretting available for the specified chord"
  in
    maybeToEither errorMessage maybeInst
```

```haskell
showPlayedInst :: PlayedInstrument -> Either Text Text
showPlayedInst (PlayedInst strings midiNote fretting)
  | length strings /= length fretting =
      Left "Number of strings and number of picks in fretting are not the same"
  | otherwise =
      Right $ "LIKE WHAT"
```


[literate programming]: https://en.wikipedia.org/wiki/Literate_programming

```haskell
main :: IO ()
main = do
  let
    output = do
      playedInst <- chordToPlayedInst "a" ukulele
      ansiArt <- showPlayedInst playedInst
      return ansiArt
  case output of
    Left error -> die error
    Right ansiArt -> Text.putStr ansiArt
```



[1]

Turns out it's a little more involved when you
want to write it in Markdown instead of LaTeX.
(Also because of bugs in
[Kramdown](https://github.com/gettalong/kramdown/issues/503) and
[Pandoc](https://github.com/jgm/pandoc/issues/4510))
But following command will execute this post in most shells:

```bash
cat 2018-03-20_cli-ukulele-fingering-chart-in-haskell.md \
| sed 's/```haskell/```{.literate .haskell}/g' \
| pandoc \
  --from markdown \
  --to markdown+lhs \
  --output temp.lhs \
| stack runhaskell \
  --resolver lts-11.1 \
  --package cmdargs \
  -- \
  temp.lhs \
; rm -f temp.lhs
```

Use `stack ghc --resolver=lts-11.1 --package=cmdargs -- -E temp.lhs`
to compile it to Haskell

All MIDI notes:

```haskell
data MidiNote
  = M00 | M01 | M02 | M03 | M04 | M05 | M06 | M07
  | M08 | M09 | M0A | M0B | M0C | M0D | M0E | M0F
  | M10 | M11 | M12 | M13 | M14 | M15 | M16 | M17
  | M18 | M19 | M1A | M1B | M1C | M1D | M1E | M1F
  | M20 | M21 | M22 | M23 | M24 | M25 | M26 | M27
  | M28 | M29 | M2A | M2B | M2C | M2D | M2E | M2F
  | M30 | M31 | M32 | M33 | M34 | M35 | M36 | M37
  | M38 | M39 | M3A | M3B | M3C | M3D | M3E | M3F
  | M40 | M41 | M42 | M43 | M44 | M45 | M46 | M47
  | M48 | M49 | M4A | M4B | M4C | M4D | M4E | M4F
  | M50 | M51 | M52 | M53 | M54 | M55 | M56 | M57
  | M58 | M59 | M5A | M5B | M5C | M5D | M5E | M5F
  | M60 | M61 | M62 | M63 | M64 | M65 | M66 | M67
  | M68 | M69 | M6A | M6B | M6C | M6D | M6E | M6F
  | M70 | M71 | M72 | M73 | M74 | M75 | M76 | M77
  | M78 | M79 | M7A | M7B | M7C | M7D | M7E | M7F
```


All intervals:

```haskell
i00 = Interval O0 S0;  i01 = Interval O0 S1;  i02 = Interval O0 S2
i03 = Interval O0 S3;  i04 = Interval O0 S4;  i05 = Interval O0 S5
i06 = Interval O0 S6;  i07 = Interval O0 S7;  i08 = Interval O0 S8
i09 = Interval O0 S9;  i10 = Interval O0 SX;  i11 = Interval O0 SE
i12 = Interval O1 S0;  i13 = Interval O1 S1;  i14 = Interval O1 S2
i15 = Interval O1 S3;  i16 = Interval O1 S4;  i17 = Interval O1 S5
i18 = Interval O1 S6;  i19 = Interval O1 S7;  i20 = Interval O1 S8
i21 = Interval O1 S9;  i22 = Interval O1 SX;  i23 = Interval O1 SE
i24 = Interval O2 S0;  i25 = Interval O2 S1;  i26 = Interval O2 S2
i27 = Interval O2 S3;  i28 = Interval O2 S4;  i29 = Interval O2 S5
i30 = Interval O2 S6;  i31 = Interval O2 S7;  i32 = Interval O2 S8
i33 = Interval O2 S9;  i34 = Interval O2 SX;  i35 = Interval O2 SE
i36 = Interval O3 S0;  i37 = Interval O3 S1;  i38 = Interval O3 S2
i39 = Interval O3 S3;  i40 = Interval O3 S4;  i41 = Interval O3 S5
i42 = Interval O3 S6;  i43 = Interval O3 S7;  i44 = Interval O3 S8
i45 = Interval O3 S9;  i46 = Interval O3 SX;  i47 = Interval O3 SE
```


All frettings:

```haskell
archaicToFretting = Map.fromList [
    ("a", [
      [[Pick F2 Middle], [Pick F1 Index], [Open], [Open]],
      [[Pick F2 Middle], [Pick F1 Index], [Open], [Pick F4 Pinky]],
      [[Pick F2 Index], [Pick F4 Ring], [Open], [Pick F4 Pinky]]
    ]),
    ("am", [
      [[Pick F2 Middle], [Open], [Open], [Open]],
      [[Pick F2 Middle], [Open], [Open], [Pick F3 SomFngr]],
      [[Pick F2 Middle], [Pick F4 SomFngr], [Open], [Pick F3 SomFngr]]
    ]),
    ("am7", [
      [[Open], [Open], [Open], [Open]],
      [[Pick F2 Middle], [Open], [Open], [Pick F3 SomFngr]],
      [[Pick F2 Middle], [Pick F4 SomFngr], [Pick F3 SomFngr], [Pick F3 SomFngr]]
    ])
    -- ("a#", [[], [], [], [], ]),
    -- ("b",  [[], [], [], [], ]),
    -- ("c",  [[], [], [], [], ]),
    -- ("c#", [[], [], [], [], ]),
    -- ("d",  [[], [], [], [], ]),
    -- ("d#", [[], [], [], [], ]),
    -- ("e",  [[], [], [], [], ]),
    -- ("f",  [[], [], [], [], ]),
    -- ("f#", [[], [], [], [], ]),
    -- ("g",  [[], [], [], [], ]),
    -- ("g#", [[], [], [], [], ]),
  ]
```


Other formattings:
```
╓───┬─M─┬───┬───┬
╟───┼───┼─R─┼───┼
╟───┼─I─┼───┼───┼
╙───┴───┴───┴───┴
╓──┬─M┬──┬──┬
╟──┼──┼─R┼──┼
╟──┼─I┼──┼──┼
╙──┴──┴──┴──┴
╓──┬Ⓜ─┬──┬──┬
╟──┼──┼Ⓡ─┼──┼
╟──┼Ⓘ─┼──┼──┼
╙──┴──┴──┴──┴
╓──┬❷─┬──┬──┬
╟──┼──┼❸─┼──┼
╟──┼❶─┼──┼──┼
╙──┴──┴──┴──┴
╓──┬⬤─┬──┬──┬
╟──┼──┼⬤─┼──┼
╟──┼⬤─┼──┼──┼
╙──┴──┴──┴──┴

   G        G
╒═╤═╤═╕  ╒═╤═╤═╕
│ │ │ │  │_│_│_│
├─┼─┼─┤  │_I_│_M
│ I │ M  │_│_R_│
├─┼─┼─┤  │_│_│_│
│ │ R │
├─┼─┼─┤
```
