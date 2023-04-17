+++
title = "How to create a bar chart from a CSV file with Haskell"
date = "2023-04-17"
author = "Adrian Sieber"

[taxonomies]
tags = ["programming", "haskell", "chart", "csv", "tutorial"]
+++

Today I wanted to create a bar chart
for a new blog post on [blog.airsequel.com](https://blog.airsequel.com).
It was supposed to show the number of days between each SQLite release.
I decided to use Haskell, but I couldn't find any good code examples out there.
So, I went ahead and wrote the code from scratch. 😮‍💨
I'm sharing it here in hopes of sparing the next person the time and effort. 😅

This is the final chart:

{{ load_file(path="days_since_last_sqlite_release.svg")}}

The data is simply copied from the
[History Of SQLite Releases](https://sqlite.org/chronology.html) page
and formated as a CSV file with the following columns:

```csv
Date,Version
2023-03-22,3.41.2
2023-03-10,3.41.1
…
```

Check out this post's
[directory at GitHub](
  https://github.com/ad-si/adriansieber-com/tree/master/content)
for the full source files.

I used [Cassandra](https://hackage.haskell.org/package/cassandra)
to parse the CSV file
and [Chart](https://hackage.haskell.org/package/Chart) to create the SVG chart.

I wanted to use [chart-svg](https://github.com/tonyday567/chart-svg) originally,
since they had just released a quite nice looking new version.
However, it's not published on Hackage or Stackage yet.
So I used the good old Chart module instead.

Without further ado, here is the code:

```hs
{{ load_file(path="build_chart.hs") }}
```

The only change I had to make to the SVG afterwards was to remove the
`width` and `height` attributes from the `<svg>` tag.
This lets it automatically scale to the size of the parent element.
I also created a ticket for them on GitHub to support omitting the size:
[github.com/timbod7/haskell-chart/issues/250.](
  https://github.com/timbod7/haskell-chart/issues/250).
