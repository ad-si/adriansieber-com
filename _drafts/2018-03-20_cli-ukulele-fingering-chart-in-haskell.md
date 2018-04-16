---
title: <code>uku</code> - A CLI tool to display Ukulele fingering charts
---

**TLDR:**
This is a tutorial on how to write a CLI tool to display fingering
charts for Ukuleles in your terminal.
As it's written in literate Haskell
the post also contains the code itself for the program.
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
and is executed like a shell script.
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
with no need for an extra compilation step.


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


Now we need types to model our domain.
Normally you'd expect something like `data Note = C | Cis | D | Dis …` here,
but this notation is only for historical reasons still used.
It actually makes not a lot of sense nowadays.
E.g. the distance between `E` and `F` is half the distance of `F` to `G` 🤦.
I'll call this notation the  "arachaic notation" for the rest of the post.

Actually, even the notion of absolute note values isn't particularly useful,
as western music is inherently relative and therefore
you can start a song from every note.
To accommodate this we simply model everything relatively
and only make the common note names and the particular tuning of the
Ukulele a special instance of it.
I'll call this notation the "relative notation".

A Piano supports 88 notes and MIDI supports 128 notes.
An octave contains 12 notes and so we can use a base 12 ([duodecimal]) system
to simplify counting in octaves.
The duodecimal system uses 2 special unicode characters for ten and eleven,
called pitman digits: `1 2 3 4 5 6 7 8 9 ↊ ↋`.
Unfortunately GHC (Glasgow Haskell Compiler) interprets them as symbols
and does not allow them in regular names.
<small>Explanation on [stackoverflow].</small>
For that reason we replace `↊` with `X`
(like the Roman literal) and `↋` with `E` (like "eleven"),
which is the recommend way for ASCII text
by the [Dozenal Society of America][duodecimal].
Each step corresponds to one archaic notation semi tone.

[duodecimal]: http://www.dozenal.org
[stackoverflow]: https://stackoverflow.com/questions/31965349/using-emoji-in-haskell

Our `Interval` data type.
The first duodecimal number after the `I`
is the octave and the second one is the semi tone.
To spare you the complete list, I append it to the end of the post.

```haskell
data Interval = I00 | I01 | I02 | … | I09 | I0X | I0E | I10 | … | IEX | IEE
```

Explicitly listing all possible intervals has the advantage that we can
now ensure at compile time that no invalid intervals are specified.

This amounts to 144 intervals, which is great, as it's
slightly more than the 128 notes defined in MIDI and therefore
we can model everything that MIDI can.

For specifying absolute note values, which you'll need at some point,
we simply use the MIDI values in dozenal notation:
`data MidiNote = M00 | M01 | M02 | … | M09 | M0X | M0E | M10 | … | MX6 | MX7`

Hereby applies that `M00` is `C-1`, `M10` is `C0` and so on.
This makes it really easy to map notes from
archaic notation to the equivalent MIDI notes and vice versa.
I appended the full list to the end of the post.

We can now map this to our Ukulele:

```txt
Archaic notation          Relative notation (to C4)    MIDI notes

A4 ╓──┬──┬──┬──┬          I09 ╓──┬──┬──┬──┬            M59 ╓──┬──┬──┬──┬
E4 ╟──┼──┼──┼──┼          I04 ╟──┼──┼──┼──┼            M54 ╟──┼──┼──┼──┼
C4 ╟──┼──┼──┼──┼          I00 ╟──┼──┼──┼──┼            M50 ╟──┼──┼──┼──┼
G4 ╙──┴──┴──┴──┴          I07 ╙──┴──┴──┴──┴            M57 ╙──┴──┴──┴──┴
```


To completely model the domain we need some more types ...
like for our fingers🤞.

```haskell
data Finger = Thumb | Index | Middle | Ring | Pinky | Any
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
The strings are listed from `i07`/`G4` to `i09`/`A4`
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
ukulele = PlayedInst [i07, i00, i04, i09] M34
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
      [[Pick F2 Middle], [Open], [Open], [Pick F3 Any]]
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
renderPicks :: [Pick] -> Text
renderPicks picks =
  let
    maxPos = picks
      & fmap (\(Pick fretPosition _) -> fretPosition)
      & Protolude.maximum
  in
    show maxPos

showPlayedInst :: PlayedInstrument -> Either Text Text
showPlayedInst (PlayedInst strings midiNote fretting)
  | length strings /= length fretting =
      Left "Number of strings and number of picks in fretting are not the same"
  | otherwise = Right $
      show (fmap renderPicks fretting)
```
ook

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
cat _drafts/2018-03-20_cli-ukulele-fingering-chart-in-haskell.md \
| sed 's/```haskell/```{.literate .haskell}/g' \
| pandoc \
  --from markdown \
  --to markdown+lhs \
  --output temp.lhs \
| stack runhaskell \
  --resolver lts-11.1 \
  --package protolude \
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
  = M00 | M01 | M02 | M03 | M04 | M05 | M06 | M07 | M08 | M09 | M0X | M0E
  | M10 | M11 | M12 | M13 | M14 | M15 | M16 | M17 | M18 | M19 | M1X | M1E
  | M20 | M21 | M22 | M23 | M24 | M25 | M26 | M27 | M28 | M29 | M2X | M2E
  | M30 | M31 | M32 | M33 | M34 | M35 | M36 | M37 | M38 | M39 | M3X | M3E
  | M40 | M41 | M42 | M43 | M44 | M45 | M46 | M47 | M48 | M49 | M4X | M4E
  | M50 | M51 | M52 | M53 | M54 | M55 | M56 | M57 | M58 | M59 | M5X | M5E
  | M60 | M61 | M62 | M63 | M64 | M65 | M66 | M67 | M68 | M69 | M6X | M6E
  | M70 | M71 | M72 | M73 | M74 | M75 | M76 | M77 | M78 | M79 | M7X | M7E
  | M80 | M81 | M82 | M83 | M84 | M85 | M86 | M87 | M88 | M89 | M8X | M8E
  | M90 | M91 | M92 | M93 | M94 | M95 | M96 | M97 | M98 | M99 | M9X | M9E
  | MX0 | MX1 | MX2 | MX3 | MX4 | MX5 | MX6 | MX7
```


All intervals:

```haskell
data Interval
  = I00 | I01 | I02 | I03 | I04 | I05 | I06 | I07 | I08 | I09 | I0X | I0E
  | I10 | I11 | I12 | I13 | I14 | I15 | I16 | I17 | I18 | I19 | I1X | I1E
  | I20 | I21 | I22 | I23 | I24 | I25 | I26 | I27 | I28 | I29 | I2X | I2E
  | I30 | I31 | I32 | I33 | I34 | I35 | I36 | I37 | I38 | I39 | I3X | I3E
  | I40 | I41 | I42 | I43 | I44 | I45 | I46 | I47 | I48 | I49 | I4X | I4E
  | I50 | I51 | I52 | I53 | I54 | I55 | I56 | I57 | I58 | I59 | I5X | I5E
  | I60 | I61 | I62 | I63 | I64 | I65 | I66 | I67 | I68 | I69 | I6X | I6E
  | I70 | I71 | I72 | I73 | I74 | I75 | I76 | I77 | I78 | I79 | I7X | I7E
  | I80 | I81 | I82 | I83 | I84 | I85 | I86 | I87 | I88 | I89 | I8X | I8E
  | I90 | I91 | I92 | I93 | I94 | I95 | I96 | I97 | I98 | I99 | I9X | I9E
  | IX0 | IX1 | IX2 | IX3 | IX4 | IX5 | IX6 | IX7


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
      [[Pick F2 Middle], [Open], [Open], [Pick F3 Any]],
      [[Pick F2 Middle], [Pick F4 Any], [Open], [Pick F3 Any]]
    ]),
    ("am7", [
      [[Open], [Open], [Open], [Open]],
      [[Pick F2 Middle], [Open], [Open], [Pick F3 Any]],
      [[Pick F2 Middle], [Pick F4 Any], [Pick F3 Any], [Pick F3 Any]]
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
```txt
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
