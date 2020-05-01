+++
title = "RosettaGit - Solutions to tasks in more than 700 programming languages"
image = "/images/logos/rosettagit.png"

[taxonomies]
tags = ["language", "website", "announcement"]
+++

**TLDR:**
[rosettagit.org] is a website which presents solutions
to the same task in as many different programming languages as possible.
It demonstrates how languages are similar and different
and can help you learn new approaches to solving problems.
It started out as a fork of [rosettacode.org]
and tries to improve on it in many ways.

[rosettagit.org]: https://rosettagit.org
[rosettacode.org]: https://rosettacode.org


## Reasons for the Fork

Since Rosetta Code caters mainly for programmers,
standard code development tooling can be safely used.
This wouldn't be possible for Wikipedia,
as a lot of less technical savvy people are collaborating on the platform.

Using the developer's default git-on-GitHub and merge request based workflow
offers a lot of advantages:

- Better diffs
- Content can be edited with code editor of choice (e.g. Vim, Emacs, …)
- Better collaboration via
  - Issues
  - Merge requests
  - Kanban boards
- Better overview of change history
- Faster loading times as the website is completely static
- Continuous integration and continuous delivery via GitHub Actions or Travis CI
    guarantees code quality and hassle free deployments of the website
  - Linting
  - Unit tests
- Less overhead as the project consists of just a repo
- Site generation only depends on the static site generator [Zola]
    <br>
    <small>
    (Which is by the way the best static site generator
    I have used so far. Really great stuff!)
    </small>
- Better design since it's not constrained by MediaWiki's features


## Log of Building the Website

In the following section I'm going to lead you through the build process
of the website.

First of all I had to download the existing Rosetta Code website.
Luckily, some fine people have already built a tool to export wikis.
You can find it at [github.com/WikiTeam/wikiteam].
They use it to archive wikis for future generations -- which is a very laudable
endeavor -- so drop by and show them some love!

[github.com/WikiTeam/wikiteam]: https://github.com/WikiTeam/wikiteam

Overall the website was 11 GB with around 13&#8239;000 pages.
Interesting about this was, how my macOS UI became basically unusable.
I had to do even the most trivial file operations via the command line 😒.

Since the WikiTeam software dropped me a 11 GB XML file (🤮) I had to split
it up to the individual pages first.
[xml-to-json-fast] <small>(Yay, Haskell 😁)</small> worked quite well,
but [jq] unfortunately encountered some Unicode problems,
which I couldn't figure out how to fix:

```txt
parse error: Invalid string: control characters from U+0000 through U+001F
must be escaped at line 3424, column 82
```

And [jl] was too slow and not streaming capable.

[xml-to-json-fast]: https://github.com/sinelaw/xml-to-json-fast
[jq]: https://github.com/stedolan/jq
[jl]: https://github.com/chrisdone/jl

This meant I had to fall back to some more ~~arcane~~ battle hardened tools:
E.g. our trusty `csplit` 🎉:

```sh
csplit -n 5 -k rosettacode_dump.xml  '/<page>/' '{999999999}'
```

It needs the `-n 5` flag for numbering, as 13&#8239;000 has 5 digits,
and the `-k` flag to not remove files if an error occurs.
The 9s specifies how many times the pattern should be repeated at the maximum.

Since the XML was pretty uniformly structured, this butchery didn't lead
to too many problems. I only had to fix 2 things:

- Clean up the first and the last file
- Normalize the `revision` field,
    since it might either be an array or an object

Then I converted everything to JSON:

```sh
ls | while read file; xml-to-json $file | jq > $file.json; end
```

Next I had to extract the page content from the JSON files and convert
it from MediaWiki syntax to [GitHub flavored markdown][gfm].

[gfm]: https://github.github.com/gfm/

For all the JSON manipulation I used of course [jq].
For converting the MediaWiki content,
I tried to use `pandoc --from mediawiki --to gfm`,
but incorrect handling of multiline code blocks put a spoke in my wheel.
So in the end, I decided it would be easier to convert the most important
parts with some command line magic and fix the rest in the future.

To get all the [jq] commands right I used the awesome [jqplay.org] website.

[jqplay.org]: https://jqplay.org

And without any further ado I present you my blob of jq and sed regex magic:

```sh
echo *.json \
| tr ' ' '\n' \
| while read file;
  echo Creating (jq -r '.page.title' $file) …
  cat $file \
  | jq -r '
    "+++\\n" +
    "title = \\"" + .page.title + "\\"\\n" +
    "description = \\"\\"\\n" +
    "date = " + .page.revision[-1].timestamp + "\\n" +
    "aliases = []\\n" +
    "[extra]\n" +
    "id = " + .page.id + "\\n" +
    "[taxonomies]\n" +
    "categories = []\\n" +
    "tags = []\\n" +
    "+++\\n\\n" +
    .page.revision[-1].text.value' \
  | gsed -E \
    -e 's/<br>/\n/g' \
    -e 's/&nbsp;/ /g' \
    -e 's/<lang ([a-zA-Z0-9 /-\+.#]+)>/\n```\1\n/g' \
    -e 's/<pre>/\n```txt\n/g' \
    -e 's/<\/(lang|pre)>/\n```\n/g' \
    -e 's/==\{\{header\|([a-zA-Z0-9 /-\+.#]+)\}\}==/\n## \1\n/g' \
    -e 's/===([a-zA-Z0-9 /-\+.#]+)===/\n### \1\n/g' \
  > (jq -r '.page.title | ascii_downcase
      | sub("( |:|/|,|;|\\\\.|\\\\+|\\\\*)"; "_"; "g")
      | sub("(\\"|\\\\$|#|\')"; ""; "g") | @uri' \
    $file).md; \
end
```

It worked quite well, although It was by far perfect.
There just comes a point where you have to accept
that it will be faster to fix all remaining errors by hand,
instead of trying to make the script perfect.

Among the resulting errors were:

- Destroyed a few places where `<pre>` and similar was part of the actual code.
- One file crashed Zola with a stack overflow due to an invalid formatting:
  [github.com/getzola/zola/issues/827]

[github.com/getzola/zola/issues/827]: https://github.com/getzola/zola/issues/827

The things I still had to do afterwards were:

- Delete all empty files and fix some escape errors like unescaped `"`.
- Delete all redirects
- Remove user and user talk pages
- Remove reports of unimplemented tasks
- Move all file meta information to corresponding files
- Delete irrelevant files with [ranger](https://ranger.github.io)
    by sorting them by size and stepping through them
- Convert snippets with
    `pandoc --from mediawiki --to gfm <path-to-file> | pbcopy`
    from MediaWiki to [GitHub Flavored Markdown][gfm]

One big mistake I made during the whole edit-compile-view loop
is that I set the `build_search_index = true` in Zola's config,
although I didn't use the index.
As building the search index is one of the most
computational intensive tasks of Zola, I wasted a lot of time glaring
at the compilation loading spinner 🤦‍♂️.
When the search index was included in the build, the whole website
took around 60 s and without it just about 15 s.
However, rebuilding the whole website was only necessary when
e.g. the base template changed.
Otherwise incremental compilation kicked in.
So it wasn't to bad …


As you can probably imagine there is still quite some work left to be done.
I hope you'll join me expanding this valuable resource!
Check out the [open issues]
or help enabling one of the many [drafts].

I'm looking forward to your feedback and contributions! 😁

[drafts]: https://rosettagit.org/drafts/
[open issues]: https://github.com/ad-si/RosettaGit/issues


[Zola]: https://www.getzola.org/
[MediaWiki]: https://www.mediawiki.org/wiki/MediaWiki
