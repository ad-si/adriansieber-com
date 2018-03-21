---
title: uku -
  A CLI tool to display Ukulele fingering charts (written in Haskell)
---

2 years ago I started to write a CLI tool to display Ukulele fingering
charts in the terminal as ANSI art.
I never found the time to finish it, but now I thought it was about time.

While I originally started to write it in JavaScript,
I recently got introduced to Haskell and I fell in love with it.
You should try it out some time.
It's a really awesome programming language!

One cool feature is that it has first class support for [literate programming].
So I'm going to try to reimplement / finish the tool in this blog post
and walk you along how I did it. [1]

First a short overview what we're actually trying to build.
This is our target output:

![Output of commmand "uku g"](/img/uku-g.svg)

You specify an accord and `uku` pretty prints it as ANSI art to the terminal.
Cool, right? So let's get started!

First some meta stuff:

``` {.literate .haskell}
{-
stack
  --resolver lts-11.1
  script
  --package cmdargs
  --package markdown-unlit
-}
```

I'm using stack's [script interpreter]
to make it an easily executable CLI script
with no need for compilation.

[script interpreter]: https://docs.haskellstack.org/en/stable/GUIDE/#script-interpreter

As dependencies we only use [Neil Mitchell's][ndmitchell] [cmdargs]
to parse CLI flags.

[ndmitchell]: https://ndmitchell.com
[cmdargs]: https://github.com/ndmitchell/cmdargs


``` {.literate .haskell}
{-# LANGUAGE DeriveDataTypeable #-}

module Uku where
import System.Console.CmdArgs

data Song = Song {accords :: String}
  deriving (Show, Data, Typeable)
```

First some imports:


[literate programming]: https://en.wikipedia.org/wiki/Literate_programming

{% ghci %}
:t myfact
myfact 10
:t myfact'
myfact' 10
{% endghci %}


``` {.literate .haskell}
-- main :: IO ()
main =
  putStr "Hello World!"
```



[1]

Turns out it's a little more involved when you
want to write it in Markdown instead of LaTeX,
but this will do to execute this post:

```shell
pandoc \
  --from markdown \
  --to markdown+lhs \
  --output temp.lhs \
  2018-03-20_cli-ukulele-fingering-chart-in-haskell.md \
| stack runhaskell \
  --resolver lts-11.1 \
  --package cmdargs \
  -- \
  temp.lhs \
; rm -f temp.lhs
```

Use `stack ghc --resolver=lts-11.1 --package=cmdargs -- -E temp.lhs`
to compile it to Haskell

