+++
title = "<code>cal</code> - Display a Calendar in Your Terminal"
image = "cal-screenshot.png"

[taxonomies]
tags = ["CLI", "cal"]
+++

<style>
  pre:nth-of-type(4),
  pre:last-of-type {
    font-size: 0.7em;
  }
</style>

![Screenshot of command line execution of `cal`](cal-screenshot.png)

This is my first post in the
[Command Line Monday](/command-line-monday) series.
It gives a short introduction to a useful command line tool every Monday.

`cal` is part of the
[Single UNIX Specification](https://en.wikipedia.org/wiki/Single_UNIX_Specification)
and <del>is</del> <ins>should</ins> therefore be installed on every
unix-like operating system.

The [official manual](
   http://pubs.opengroup.org/onlinepubs/9699919799/utilities/cal.html)
of `cal` in the unix specification defines only 3 commands:

```txt
$ cal
      May 2016
Su Mo Tu We Th Fr Sa
 1  2  3  4  5  6  7
 8  9 10 11 12 13 14
15 16 17 18 19 20 21
22 23 24 25 26 27 28
29 30 31
```

```txt
$ cal 2015
                             2015

      January               February               March
Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa
             1  2  3   1  2  3  4  5  6  7   1  2  3  4  5  6  7
 4  5  6  7  8  9 10   8  9 10 11 12 13 14   8  9 10 11 12 13 14
11 12 13 14 15 16 17  15 16 17 18 19 20 21  15 16 17 18 19 20 21
18 19 20 21 22 23 24  22 23 24 25 26 27 28  22 23 24 25 26 27 28
25 26 27 28 29 30 31                        29 30 31

       April                  May                   June
Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa
          1  2  3  4                  1  2      1  2  3  4  5  6
 5  6  7  8  9 10 11   3  4  5  6  7  8  9   7  8  9 10 11 12 13
12 13 14 15 16 17 18  10 11 12 13 14 15 16  14 15 16 17 18 19 20
19 20 21 22 23 24 25  17 18 19 20 21 22 23  21 22 23 24 25 26 27
26 27 28 29 30        24 25 26 27 28 29 30  28 29 30
                      31
        July                 August              September
Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa
          1  2  3  4                     1         1  2  3  4  5
 5  6  7  8  9 10 11   2  3  4  5  6  7  8   6  7  8  9 10 11 12
12 13 14 15 16 17 18   9 10 11 12 13 14 15  13 14 15 16 17 18 19
19 20 21 22 23 24 25  16 17 18 19 20 21 22  20 21 22 23 24 25 26
26 27 28 29 30 31     23 24 25 26 27 28 29  27 28 29 30
                      30 31
      October               November              December
Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa
             1  2  3   1  2  3  4  5  6  7         1  2  3  4  5
 4  5  6  7  8  9 10   8  9 10 11 12 13 14   6  7  8  9 10 11 12
11 12 13 14 15 16 17  15 16 17 18 19 20 21  13 14 15 16 17 18 19
18 19 20 21 22 23 24  22 23 24 25 26 27 28  20 21 22 23 24 25 26
25 26 27 28 29 30 31  29 30                 27 28 29 30 31
```

```txt
$ cal 9 2015
   September 2015
Su Mo Tu We Th Fr Sa
       1  2  3  4  5
 6  7  8  9 10 11 12
13 14 15 16 17 18 19
20 21 22 23 24 25 26
27 28 29 30
```

Simple but useful. That's what we love about Unix.

Real world implementations which can be found on Mac OS, BSD
and of course GNU/Linux are, however, much more powerful and have some nifty
additional features.
We'll have closer look at the GNU version, also called `gcal`.
It's installed on Ubuntu per default and can
be installed on Mac OS with [Homebrew](brew.sh):
`brew install gcal`

One very useful command for example is
`gcal --holiday-list --cc-holiday=<location-code>` or the short form
`gcal -n -q <location-code>`,
where `<location-code>` is a two letter country code
plus (sometimes) a two letter territory code.

Example output:

```txt
$ gcal --holiday-list --cc-holidays=us_ca

Eternal holiday list:                      The year 2016 is A leap year

New Year's Day (US_CA)                   + Fri,  Jan: 1st:2016 =  -51 days
Martin L. King's Day (US_CA)             + Mon,  Jan:18th:2016 =  -34 days
Groundhog Day (US_CA)                    - Tue,  Feb  2nd 2016 =  -19 days
President Lincoln's Birthday (US_CA)     + Fri,  Feb:12th:2016 =   -9 days
St Valentine's Day (US_CA)               - Sun,  Feb 14th 2016 =   -7 days
Presidents' Day (US_CA)                  + Mon,  Feb:15th:2016 =   -6 days
St Patrick's Day (US_CA)                 - Thu,  Mar 17th 2016 =  +25 days
Good Friday (US_CA)                      * Fri,  Mar 25th 2016 =  +33 days
Cesar Chavez Day (US_CA)                 + Thu,  Mar:31st:2016 =  +39 days
All Fool's Day (US_CA)                   - Fri,  Apr  1st 2016 =  +40 days
Prayer Day (US_CA)                       - Thu,  May  5th 2016 =  +74 days
Nurses' Day (US_CA)                      - Fri,  May  6th 2016 =  +75 days
Mother's Day (US_CA)                     - Sun,  May  8th 2016 =  +77 days
Armed Forces Day (US_CA)                 - Sat,  May 21st 2016 =  +90 days
Remembrance/Memorial Day (US_CA)         + Mon,  May:30th:2016 =  +99 days
Father's Day (US_CA)                     - Sun,  Jun 19th 2016 = +119 days
Independence Day (US_CA)                 + Mon,  Jul: 4th:2016 = +134 days
Parent's Day (US_CA)                     - Sun,  Jul 24th 2016 = +154 days
Friendship Day (US_CA)                   - Sun,  Aug  7th 2016 = +168 days
Labour Day (US_CA)                       + Mon,  Sep: 5th:2016 = +197 days
Admission Day (US_CA)                    + Fri,  Sep: 9th:2016 = +201 days
Grandparents' Day (US_CA)                - Sun,  Sep 11th 2016 = +203 days
Citizenship Day (US_CA)                  - Sat,  Sep 17th 2016 = +209 days
Children's Day (US_CA)                   - Sun,  Oct  9th 2016 = +231 days
Columbus Day (US_CA)                     + Mon,  Oct:10th:2016 = +232 days
Sweetest Day (US_CA)                     - Sat,  Oct 15th 2016 = +237 days
Bosses' Day (US_CA)                      - Sun,  Oct 16th 2016 = +238 days
Mother in Law's Day (US_CA)              - Wed,  Oct 26th 2016 = +248 days
Halloween (US_CA)                        - Mon,  Oct 31st 2016 = +253 days
Veteran's Day (US_CA)                    + Fri,  Nov:11th:2016 = +264 days
Thanksgiving Day (US_CA)                 + Thu,  Nov:24th:2016 = +277 days
Thanksgiving Day (US_CA)                 + Fri,  Nov:25th:2016 = +278 days
Christmas Day (US_CA)                    + Mon,  Dec:26th:2016 = +309 days
Kwanzaa (US_CA)                          - Mon,  Dec 26th 2016 = +309 days
Kwanzaa (US_CA)                          - Tue,  Dec 27th 2016 = +310 days
Kwanzaa (US_CA)                          - Wed,  Dec 28th 2016 = +311 days
Kwanzaa (US_CA)                          - Thu,  Dec 29th 2016 = +312 days
Kwanzaa (US_CA)                          - Fri,  Dec 30th 2016 = +313 days
Kwanzaa (US_CA)                          - Sat,  Dec 31st 2016 = +314 days
```

There are also countless options to change the display range
and the output formatting.

- `gcal .` - Display the last, the current and the next month
- `gcal -j` - Display the days as day of the year instead of day of the month
  (e.g. 342 instead of 7)
- `gcal 3-7` - Display days of month 3 to 7
- …

Time range and filtering options can of course also be combined.

Another interesting feature is gcals ability to load calendar entries
from a so called resource file. Basically it's a list of dates and
a corresponding text.
To try it out create the file `~/.gcalrc` with following content:

```txt
20160428 1. Write your first "Command Line Monday" post!
00000428 It's John's %B1987  Birthday

0 Today is %>1*K , %>02&*D  %U  %Y !
0 It's the %>03&*N  day of the year.
0 The week number is: %k
0 It's %t*  o'clock, Mr. %-USER
0 Hurry up with your work,\
 sunrise is at %o+5158+00738+61,2: .
0 Moon phase %>03*O ~Text %Z
```

If you now run `gcal --today` and it was 2016-04-28 or you specify the date with
`gcal --list-of-fixed-dates %20160428` (short `gcal -c %20160428`)
you get following result:

```txt
$ gcal --list-of-fixed-dates %20160428

Fixed date list:

Thu,  Apr<28th>2016: 1. Write your first "Command Line Monday" post!
Thu,  Apr<28th>2016: Hurry up with your work, sunrise is at 06:03.
Thu,  Apr<28th>2016: It's 09:56am o'clock, Mr. adrian
Thu,  Apr<28th>2016: It's John's 29 Birthday
Thu,  Apr<28th>2016: It's the 119th day of the year.
Thu,  Apr<28th>2016: Moon phase 066%-
                     Text
                                @@@@@@@@@@   )
                           @@@@@@@@@@@@@@@@@@     )
                         @@@@@@@@@@@@@@@@@@@@@      )
                       @@@@@@@@@@@@@@@@@@@@@@@@       )
                      @@@@@@@@@@@@@@@@@@@@@@@@@        )
                     @@@@@@@@@@@@@@@@@@@@@@@@@@@        )
                     @@@@@@@@@@@@@@@@@@@@@@@@@@@        )
                      @@@@@@@@@@@@@@@@@@@@@@@@@        )
                       @@@@@@@@@@@@@@@@@@@@@@@@       )
                        @@@@@@@@@@@@@@@@@@@@@@       )
                           @@@@@@@@@@@@@@@@@@     )
                               @@@@@@@@@@@@   )
Thu,  Apr<28th>2016: The week number is: 17
Thu,  Apr<28th>2016: Today is Thursday, 28th April 2016!
```

As you can see gcal has some crazy features.
And displaying the current moon phase
as well calculating the current sunrise and sunset times are just the beginning.
There is even a feature to adjust the sunrise time by specifying the current
atmospheric air pressure. 😯
This, however, is completely out of the scope of this post.
If you want to dig deeper, I recommend you to check out the official
[gcal manual](https://www.gnu.org/software/gcal/manual/gcal.html).
It's quite a read.

I'd be really excited to hear all the use cases for gcal one can come up with.
Seems really suited for a TODO list manager
or to never forget a birthday again.
Maybe even as a ssh-synced shared company calendar app
where every employee has its own resource file.

**Let me hear your thoughts on this!**

I hope you enjoyed my first post of the "Command Line Monday" series.
Make sure to come back next Monday for a new post!
If you are afraid that you might miss it you can
go to the [landing page](/) and sign up for my newsletter
to get a friendly reminder!


## Update 2016-05-16

This post got unexpectedly quite some traction on
[Hacker News](https://news.ycombinator.com/item?id=11665909) 😁
and therefore I just wanted to highlight a few of the
interesting things people mentioned in the comments.

A lot of people [thought](https://news.ycombinator.com/item?id=11677784)
that Monday should be the first day of the week
and I completely agree!
It's the official ISO 8601 standard after all.
<small>
  (And as you can read in [Germany — You have failed the metric system](
  /germany-you-have-failed-the-metric-system), standards mean a lot to me!)
</small>

For convenience I use following script on my mac to alias `cal`:

```bash
#! /usr/bin/env bash

gcal --starting-day=1 "$@"
```

This uses Monday as the first day of the week.
Problem solved 😊

[Bushra8](https://news.ycombinator.com/user?id=Bushra8) also
[mentioned](https://news.ycombinator.com/item?id=11678096)
a cool flag which displays the astronomical holidays.
I.e. the next full moon or the next lunar eclipse.

```shell
$ gcal --holiday-list --astronomical-holidays

Eternal holiday list:                      The year 2016 is A leap year

New Moon 01:30 (Ast)                     - Sun,  Jan 10th 2016 = -127 days
Waxing Half Moon 23:26 (Ast)             - Sat,  Jan 16th 2016 = -121 days
Full Moon 01:46 (Ast)                    - Sun,  Jan 24th 2016 = -113 days
Waning Half Moon 03:28 (Ast)             - Mon,  Feb  1st 2016 = -105 days
New Moon 14:39 (Ast)                     - Mon,  Feb  8th 2016 =  -98 days
Waxing Half Moon 07:46 (Ast)             - Mon,  Feb 15th 2016 =  -91 days
Full Moon 18:20 (Ast)                    - Mon,  Feb 22nd 2016 =  -84 days
Waning Half Moon 23:11 (Ast)             - Tue,  Mar  1st 2016 =  -76 days
New Moon 01:54 (Ast)                     - Wed,  Mar  9th 2016 =  -68 days
Solar Eclipse/Total 01:57 (Ast)          - Wed,  Mar  9th 2016 =  -68 days
Waxing Half Moon 17:03 (Ast)             - Tue,  Mar 15th 2016 =  -62 days
Equinox Day 04:30 (Ast)                  - Sun,  Mar 20th 2016 =  -57 days
Full Moon 12:01 (Ast)                    - Wed,  Mar 23rd 2016 =  -54 days
Lunar Eclipse/Penumbral 11:47 (Ast)      - Wed,  Mar 23rd 2016 =  -54 days
Waning Half Moon 15:17 (Ast)             - Thu,  Mar 31st 2016 =  -46 days
…
```
