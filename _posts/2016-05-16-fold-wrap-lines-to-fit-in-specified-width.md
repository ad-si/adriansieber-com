---
title: <code>fold</code> - Wrap lines to fit in specified width
image: /img/fold-screenshot.png
---

![Screenshot of command line execution of `fold`](/img/fold-screenshot.png)

This is the second post in my
[Command Line Monday](/command-line-monday) series.

On Unix-like operating systems there is a large number of
[command line tools for text-processing](
   https://en.wikipedia.org/wiki/List_of_Unix_commands).
One of this tools is called `fold` and helps you to wrap
lines to a certain length.

`fold` is part of the
[Single UNIX Specification
](https://en.wikipedia.org/wiki/Single_UNIX_Specification)
and is therefore already pre-installed on every
unix-like operating system.

The [official specification](
   http://pubs.opengroup.org/onlinepubs/9699919799/utilities/fold.html)
describes 3 command line flags and mandates reading support
from input files and stdin.

The easiest way to use `fold` is to simply call it on a file.

```shell
$ fold example.txt
```

This will wrap every line of the file
to the default line length for Unix terminals of 80 characters
and print the wrapped line to stdout.
From there it can easily be redirected into a new file or
to another command line tool for further processing.

![Screenshot of command line output of `fold example.txt`](
   /img/fold-file-screenshot.png)

As you may have noticed, `fold` doesn't care about word boundaries and simply
cuts off words when the maximum number of characters is reached.
By specifying the `-s` flag `fold` can be set up to wrap the line
at the closest preceding whitespace character.

![Screenshot of command line output of `fold -s example.txt`](
   /img/fold-file-spaces-screenshot.png)

If you don't want to use the default terminal width for wrapping
you can set up a custom one with the `-w` flag and
also combine it with the `-s` flag:

![Screenshot of command line output of `fold -w 40 -s example.txt`](
   /img/fold-file-40-spaces-screenshot.png)

And last but not least the command line flag `-b` lets `fold` count
bytes rather than characters.
This can be especially useful if the input contains tabs
and the tab characters shall be counted instead of the number of spaces they
represent.

![Screenshot of command line output of `fold` with `-b` flag](
   /img/fold-file-bytes-screenshot.png)


As a final note I recommend to also check out the
[GNU version of `fold`](
  https://www.gnu.org/software/coreutils/manual/coreutils.html#fold-invocation).
As always, it provides a saner syntax including long flags
(`--width` in addition to `-w`, `--spaces` for `-s` and `--bytes` for `-b`)
and is actively maintained by the GNU community.
Therefore it's also more stable and secure than other implementations.
