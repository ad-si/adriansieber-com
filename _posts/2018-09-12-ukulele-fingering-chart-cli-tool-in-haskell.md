---
title: <code>uku</code> -
  A Haskell CLI tool to display Ukulele fingering charts
image: /img/uku-g.svg
---

**TLDR:**
This is a tutorial on how to write a CLI tool in Haskell to display fingering
charts for the Ukulele in your terminal.
As this post is written in [literate Haskell],
it also contains the complete code for the program itself.
If you just want to use the tool
check out the [short how to](#literate-haskell-how-to) at the end.

[literate Haskell]: https://en.wikipedia.org/wiki/Literate_programming

While I originally started to write this 2 years ago
(I thought it's about time to finish it 😛)
in JavaScript, I recently got introduced to Haskell and it's awesome.
[Especially for building CLI tools][cli-tools].

[cli-tools]:
  https://github.com/Gabriel439/post-rfc/blob/master/sotu.md#scripting--command-line-applications

First a short overview of what it's actually supposed to do.
This is our target output:

![Output of command "uku g"](/img/uku-g.svg)

You specify a chord and `uku` pretty prints the fingering chart
as an [ANSI art chord box] to the terminal.
Cool, right? So let's get started with the code:

[ANSI art chord box]: https://www.justinguitar.com/en/BC-108-TABandBoxes.php

First, we need to set a few compiler settings and import some basic modules:

```haskell
{-# OPTIONS_GHC -Wall -Wincomplete-uni-patterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

module Main where

import Protolude as Pl

import Data.List.Index (setAt, imap)
import Data.Map.Strict as Map
import Data.Text as Text
import Unsafe (unsafeHead)
```

Now we need types to model our domain.
Normally you'd expect something like `data Note = C | Cis | D | Dis …`,
but this notation is mostly used for historical reasons
and not for its ingeniousness.
E.g. the distance between `E` and `F` is half the distance of `F` to `G` 🤦.
The notation just doesn't make a lot of sense
in times of the twelve-tone [equal temperament].
I'll call this notation the  "arachaic notation" for the rest of the post.

[equal temperament]: https://en.wikipedia.org/wiki/Equal_temperament

Actually, even the notion of absolute note values isn't particularly useful,
as western music is inherently relative
and you can start the same song from every note.
To accommodate this we simply model everything relatively
and only make the common note names and the particular tuning of the
Ukulele a special instance of it.
I'll call this notation the "relative notation".

A Piano supports 88 notes and MIDI supports 128 notes.
An octave contains 12 notes and so we can use a base 12 ([duodecimal]) system
to simplify counting in octaves.
The duodecimal system uses 2 special unicode characters for ten and eleven,
called pitman digits: `0 1 2 3 4 5 6 7 8 9 ↊ ↋`.
Unfortunately [GHC] interprets them as symbols
and does not allow them in regular names
<small>(Explanation on [stackoverflow])</small>.
For that reason we replace `↊` with `X`
(like the Roman literal for 10) and `↋` with `E` (like "eleven"),
which is the recommend way for ASCII text
by the [Dozenal Society of America][duodecimal].
Each step corresponds to one semi tone in archaic notation.

[GHC]: https://en.wikipedia.org/wiki/Glasgow_Haskell_Compiler
[duodecimal]: http://www.dozenal.org
[stackoverflow]:
  https://stackoverflow.com/questions/31965349/using-emoji-in-haskell

Our `Interval` data type:

```haskell
-- data Interval
--   = I00 | I01 | I02 | … | I09 | I0X | I0E
--   | I10 | …                         | IEE
```

The first duodecimal number after the `I`
is the octave and the second one is the semi tone.
To spare you the complete list, I appended it to the end of the post.

Explicitly listing all possible intervals has the advantage that we can
now ensure at compile time that no invalid intervals are specified.

This amounts to 144 intervals, which is great, as it's
slightly more than the 128 notes defined in MIDI and therefore
we can model everything that MIDI can.

For specifying absolute note values, which we'll need at some point,
we simply use the MIDI values in dozenal notation:

```haskell
-- data MidiNote
--   = M00 | M01 | M02 | … | M09 | M0X | M0E
--   | M10 |                         … | MX7
```

This can easily be translated to the archaic notation,
as `M00` is C-1, `M10` is C0, `M20` is C1 and so on.
I appended the full list to the end of the post.

We can now map this to our Ukulele:

```
Archaic notation    Relative to C4       Absolute MIDI notes

A4 ╓──┬──┬──┬──┬    I09 ╓──┬──┬──┬──┬    M59 ╓──┬──┬──┬──┬
E4 ╟──┼──┼──┼──┼    I04 ╟──┼──┼──┼──┼    M54 ╟──┼──┼──┼──┼
C4 ╟──┼──┼──┼──┼    I00 ╟──┼──┼──┼──┼    M50 ╟──┼──┼──┼──┼
G4 ╙──┴──┴──┴──┴    I07 ╙──┴──┴──┴──┴    M57 ╙──┴──┴──┴──┴
```


To completely model the domain we need some more types ...
like for our 5 fingers 🤚.

```haskell
data Finger = Thumb | Index | Middle | Ring | Pinky | AnyFinger
  deriving (Eq, Ord, Show)
```

Each finger will later be rendered by printing its first character.
We also add ANSI color codes to colorize the terminal output.

```haskell
fingerToText :: Finger -> Text
fingerToText finger =
  let colorize text = "\x1b[31m" <> text <> "\x1b[0m"
  in colorize $ case finger of
    Thumb     -> "T"
    Index     -> "I"
    Middle    -> "M"
    Ring      -> "R"
    Pinky     -> "P"
    AnyFinger -> "●"
```

Now we need to make it possible to pick a string at a certain position,
play the string open, or mute the string
<small>(Attention: "Strings" always refers to the Ukulele strings.
The datatype to store a string of characters is called `Text`)</small>.

To model the pick position we'll have to define a fret position
in the range 1 to _length of fretboard_.
There is, however, no good type safe way to model this with integers.
0 could mean open, but using a special value for it makes more sense.
Normally fretted instruments don't have more than around 30 frets,
so we'll just use the base 36 system without the zero (i.e. 1, …, 9, A, …, Z).

```haskell
data FretPosition
  =      F1 | F2 | F3 | F4 | F5 | F6 | F7 | F8 | F9 | FA | FB
  | FC | FD | FE | FF | FG | FH | FI | FJ | FK | FL | FM | FN
  | FO | FP | FQ | FR | FS | FT | FU | FV | FW | FX | FY | FZ
  deriving (Bounded, Enum, Eq, Ord, Show)
```

`Mute` indicates to not play the string, `Open` means without picking it,
and `Pick` defines the fretboard position
and the finger to perform the pick with.

```haskell
data Pick
  = Mute
  | Open
  | Pick FretPosition Finger
  deriving (Eq, Ord, Show)
```

To be able to perform calculations with fretboard positions
we define a function to convert a `Pick` to an `Int`.

```haskell
pickToInt :: Pick -> Int
pickToInt fretPosition =
  case fretPosition of
      Pick fret _ -> (fromEnum fret) + 1
      _ -> 0
```

One complete fretting of a chord is defined as:

```haskell
type Fretting = [[Pick]]
```

Explanation:
Several fingers can pick one string and that for each string.
The strings are listed
from `I07`/`G4` (placed at the top of the fretboard)
to `I09`/`A4` (placed at the bottom).
That's what your fretboard looks like when you look at the Ukulele
as depicted in the chord boxes from above.

Finally we define a played fretted instrument by a list of relative notes
for all strings, it's base note (the note of the lowest string)
and the current fingering / fretting pattern.

```haskell
type InstStrings = [Interval]
data PlayedInstrument = PlayedInst InstStrings MidiNote Fretting
type Instrument = Fretting -> PlayedInstrument
```

The cool thing about this representation is that if you want to change
the tuning of the instrument
(e.g. by using a [capo](https://en.wikipedia.org/wiki/Capo))
or want to use a differntly tuned Ukulele
you only have to change the base note, instead of changing each string.

Based on this `PlayedInstrument` data type
we can define a normal Ukulele by partially applying
the data constructor like this:

```haskell
ukulele :: Instrument
ukulele = PlayedInst [I07, I00, I04, I09] M34
```

I added the type signature to make it clearer.
Our ukulele is now an `Instrument`,
aka a function which gets applied to a finger pattern
and returns a played instrument.
Makes sense, right?

And now we can finally play our first chords on the ukulele 🎉.
For example G major:

```haskell
gMajor :: PlayedInstrument
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

Or B major
(Formatted as 4 columns
to show the relation between the datatype format and the output.):

```haskell
bMajor :: PlayedInstrument
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

Although we've now invented a more logical musical notation system,
we still need a way to map from the archaic notation to the fingering patterns.
We could try to write a function to automatically generate all of them,
but deciding which finger to use for which pick
is based on the anatomy of hands and it would be really hard to model this.
(Feel free to prove me wrong 😉.)
So instead of overengineering it,
we'll stick to a simple manually generated lookup table.

I'll give you a short excerpt of the map,
so you know what it looks like
and move the complete map to the end of the post.
Note that for each chord there are several ways how the chord can be picked.
(Sorted from most to least common.)

```haskell
archaicToFrettingA :: Map Text [Fretting]
archaicToFrettingA = Map.fromList [
    ("a", [
      [[Pick F2 Middle], [Pick F1 Index], [Open], [Open]],
      [[Pick F2 Middle], [Pick F1 Index], [Open], [Pick F4 Pinky]]
    ]),
    ("am", [
      [[Pick F2 Middle], [Open], [Open], [Open]],
      [[Pick F2 Middle], [Open], [Open], [Pick F3 AnyFinger]]
    ])
  ]
```


The next step is to write a set of functions
to render the fretting model to our ANSI art chart boxes.
This only works if the chord is defined.

Generate the played instrument:

```haskell
chordToPlayedInsts ::
  Text -> Instrument -> Either Text [PlayedInstrument]
chordToPlayedInsts chord instrument =
  let
    maybeInst = do
      frettings <- Map.lookup chord archaicToFretting
      pure $ fmap instrument frettings
    errorMessage =
      "There is no fretting available for the specified chord"
  in
    maybeToEither errorMessage maybeInst
```

Render the pick onto an open string text:

```haskell
putPickOnString :: Pick -> [Text] -> [Text]
putPickOnString pick stringParts =
  case pick of
    Mute -> stringParts
    Open -> stringParts
    (Pick _ finger) ->
      setAt
        (pickToInt pick)
        (fingerToText finger)
        stringParts
```

Get the rendering of each string:

```haskell
getString :: Int -> Int -> Int -> [Pick] -> [Text]
getString numberOfFrets numOfStrings stringIndex strPick =
  let
    openString = [(if
      | stringIndex == 0                  -> "╒"
      | stringIndex == (numOfStrings - 1) -> "╕"
      | otherwise                         -> "╤")]
      <> Pl.replicate (numberOfFrets + 1) "│"
  in
    Pl.foldr putPickOnString openString strPick
```

Render the complete fretting:

```haskell
showFretting :: Fretting -> Text
showFretting fretting =
  let
    maxPos = Pl.maximum $ fmap pickToInt $ fold fretting
  in
    fretting
      & imap (getString maxPos $ Pl.length fretting)
      & Pl.intersperse (["═"] <> Pl.replicate (maxPos + 1) "_")
      & Pl.transpose
      & Pl.intercalate ["\n"]
      & fold
      & (<> "\n")
```

Render the played instrument
(Only works when number of strings per pick match with the number of strings
of the instrument):

```haskell
showPlayedInst :: PlayedInstrument -> Either Text Text
showPlayedInst (PlayedInst strings _ fretting)
  | Pl.length strings /= Pl.length fretting =
      Left "Number of strings and picks in fretting do not match"
  | otherwise = Right $
      showFretting fretting
```

Render it for each possible fretting for a certain chord:

```haskell
getAnsiArts :: Text -> Either Text Text
getAnsiArts chord = do
  playedInsts <- chordToPlayedInsts chord ukulele
  fmap
    (Text.intercalate "\n")
    (mapM showPlayedInst playedInsts)
```

Finally a short `main` function to define the command line interface
and handle I/O.

```haskell
main :: IO ()
main = do
  chords <- getArgs
  if
    | Pl.length chords < 1 -> die "Usage: uku <chord>"
    | Pl.length chords > 1 -> die "Supportrs only 1 chord per call"
    | otherwise            ->
        case (getAnsiArts $ pack $ unsafeHead chords) of
          Left error -> die error
          Right ansiArt -> putStr $ ansiArt <> "\n"
```

And there we go:
You now have a simple CLI tool to render Ukulele fingering charts.
If you're looking for a challenge you could now extend it to Guitars!

Hope you liked it and if so, feel free to [subscribe to my newsletter](/)
to get an email when I publish a new post! 😁

---

<a name="literate-haskell-how-to">How to execute literate Haskell:</a>

Turns out it's a little more involved when you
want to write it in Markdown instead of LaTeX
(Also because of issues with
[Kramdown](https://github.com/gettalong/kramdown/issues/503) and
[Pandoc](https://github.com/jgm/pandoc/issues/4510)).
But following command will execute this post in most shells:

```bash
curl --silent \
  http://code.adriansieber.com/adrian/adriansieber-com/raw/branch/master/_posts/2018-09-12_cli-ukulele-fingering-chart-in-haskell.md \
| sed 's/```haskell/```{.literate .haskell}/g' \
| pandoc \
  --from markdown \
  --to markdown+lhs \
  --output temp.lhs \
| stack runhaskell \
  --resolver lts-12.9 \
  --package protolude \
  --package ilist \
  -- temp.lhs a
```

Or to compile it to a `uku` executable replace the last part with:

```bash
…
| stack ghc \
  --resolver lts-12.9 \
  --package protolude \
  --package ilist \
  -- temp.lhs -o uku
```

---

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
  deriving (Eq, Ord, Show)
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
  deriving (Eq, Ord, Show)
```


All frettings:

```haskell
archaicToFretting :: Map Text [Fretting]
archaicToFretting = Map.fromList [
    ("a", [
      [[Pick F2 Middle], [Pick F1 Index], [Open], [Open]],
      [[Pick F4 Index, Pick F6 Ring],
        [Pick F4 Index], [Pick F4 Index, Pick F5 Middle], [Pick F4 Index]]
    ]),
    ("am", [
      [[Pick F2 Middle], [Open], [Open], [Open]]
    ]),

    ("a#", [
      [[Pick F1 Index, Pick F3 Ring],
        [Pick F1 Index, Pick F2 Middle], [Pick F1 Index], [Pick F1 Index]]
    ]),
    ("a#m", [
      [[Pick F1 Index, Pick F3 Ring],
        [Pick F1 Index], [Pick F1 Index], [Pick F1 Index]]
    ]),

    ("b", [
      [[Pick F2 Index, Pick F4 Ring],
        [Pick F2 Index, Pick F3 Middle], [Pick F2 Index], [Pick F2 Index]]
    ]),
    ("bm", [
      [[Pick F2 Index, Pick F4 Ring],
        [Pick F2 Index], [Pick F2 Index], [Pick F2 Index]]
    ]),

    ("c", [[[Open], [Open], [Open], [Pick F3 Ring]]]),
    ("cm", [[[Open], [Pick F3 Index], [Pick F3 Index], [Pick F3 Index]]]),

    ("c#", [
      [[Pick F1 Index],
        [Pick F1 Index], [Pick F1 Index], [Pick F1 Index, Pick F4 Pinky]
      ]
    ]),
    ("c#m", [[[Pick F1 Index], [Pick F1 Index], [Open], [Open]]]),

    ("d",  [[[Pick F2 Index], [Pick F2 Middle], [Pick F2 Middle], [Open]]]),
    ("dm",  [[[Pick F2 Middle], [Pick F2 Ring], [Pick F1 Index], [Open]]]),

    ("d#", [[[Open], [Pick F3 Ring], [Pick F3 Pinky], [Pick F1 Index]]]),
    ("d#m", [
      [[Pick F3 Ring], [Pick F3 Pinky], [Pick F2 Middle], [Pick F1 Index]]
    ]),

    ("e",  [[[Pick F1 Index], [Pick F4 Pinky], [Open], [Pick F2 Middle]]]),
    ("em",  [[[Open], [Pick F4 Ring], [Pick F3 Middle], [Pick F2 Index]]]),

    ("f",  [[[Pick F2 Middle], [Open], [Pick F1 Index], [Open]]]),
    ("fm",  [[[Pick F1 Index], [Open], [Pick F1 Middle], [Pick F3 Pinky]]]),

    ("f#", [
      [[Pick F1 Index, Pick F3 Ring],
        [Pick F1 Index], [Pick F1 Index, Pick F2 Middle], [Pick F1 Index]
      ]
    ]),
    ("f#m", [[[Pick F2 Middle], [Pick F1 Index], [Pick F2 Ring], [Open]]]),

    ("g",  [[[Open], [Pick F2 Index], [Pick F3 Ring], [Pick F2 Middle]]]),
    ("gm",  [[[Open], [Pick F2 Middle], [Pick F3 Ring], [Pick F1 Index]]]),

    ("g#", [
      [[Pick F3 Index, Pick F5 Ring],
        [Pick F3 Index], [Pick F3 Index, Pick F4 Middle], [Pick F3 Index]
      ]
    ]),
    ("g#m", [
      [[Pick F4 Ring], [Pick F3 Middle], [Pick F4 Pinky], [Pick F2 Index]]
    ])
  ]
```
