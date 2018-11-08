---
title: Comma first is bullshit
...

- No sorting as bracket is in the way
- No meaningful diffs
- Most unimportant characters first
- Humans first, tools second
- Missing commas must be dealt with by your IDE
- Quite difficult to add a new first item
- Quite difficult to change order if first item is involved
- Best would be no commas at all (CoffeeScript, ...)
- Terminators are better than separators
- It's not possible to simply comment out the first line
- `git blame` gets invalidated

Why is a special Syntax necessary in Haskell?

```haskell
colorsA =
  "Red" :
  "Green" :
  "Blue" :
  []
colorsB = ["Red", "Green", "Blue"]
```

Why numbering should start at zero:
https://www.cs.utexas.edu/users/EWD/transcriptions/EWD08xx/EWD831.html

There was already a war about semicolons as statement
separators or terminators during 1960 - 1980.

Use following issues as first draft:
https://github.com/dhall-lang/dhall-lang/issues/66

https://wiki.haskell.org/Syntactic_sugar


> One generally accepted experimental result in programmer psychology
> is that semicolon as separator is about ten times more prone to error
> than semicolon as terminator

- http://www.cs.virginia.edu/~cs655/readings/bwk-on-pascal.html

- http://wiki.c2.com/?SemiColon
