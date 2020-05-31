+++
title = "Literate Patching"
draft = true

[taxonomies]
tags = ["programming"]
+++

In 1984 Donald Knuth released the seminal work "[Literate Programming]",
which describes the novel "literate programming paradigm".
It's defined as the combination of documentation and source code
in a fashion suited for reading by human beings.

In its original incarnation the accompanying implementation
called "WEB" used Pascal as its underlying programming language,
TeX for typesetting of the documentation, and PDF as the output format.
Later, Donald Knuth and Silvio Levy introduced a successor called "[CWEB]"
which uses C instead of Pascal.

{% figure(
    src="math_matrix_snippet.jpeg",
    alt="Snippet of a literate code implementation
      of a C++ matrix algebra library"
) %}
  Snippet of a CWEB based literate code implementation
  of a C++ matrix algebra library <br>
  demonstrating advanced math rendering, code listings, and cross references
{% end %}

For a more modern example of the same concept,
also check out my [post about implementing a small CLI tool][uku-post],
which uses Haskell embedded in Markdown and compiles to HTML.

While I am a big fan of the concept and have used it numerous times,
I feel like it's not well suited for projects larger than a blog post
with a few hundred lines of code.

Literate programming excels when you want to document the current state
and functionality of a program. However, for larger and therefore
necessarily older programs it's often just as important to understand
how it evolved to the current state and which constraints shaped the final
result.

Sure, you could extend the documentation part of a code snippet
with a historical account of when and why it was implemented and what
alternatives were considered and the reasoning for not choosing them.
But this will likely hurt readability and not necessarily be relevant for
everyone. In a nutshell, there are 2 different goals of reading code:
How does it work, and why does it work this way?
Literate programming only lends itself to the first question.

In order to better answer the question "why does it work this way?"
I am proposing a new concept called "Literate Patching",
where a code project is understood as a sequence
of self-contained commented code patches.

In a way a standard git repository can already be understood as a
literate patch project,
where each commit describes a patch documented by a commit message
and where the concatenation of all commits yields the final project.
However, this implementation lacks crucial aspects:

- Commit messages document all patches of a commit combined.
    There is no way to document each change of a commit
    (also called hunk) individually.

- Commits aren't rendered and therefore can't include valuable
    media like images, math formulas, etc.

- Commits aren't necessarily self-contained.
    E.g. if a commit adds a new function with a bug
    and another commit fixes this bug in the future,
    the complete change is split over 2 commits.
    This could be circumvented by rewriting the history and combining
    the 2 commits into one.
    But doing so safely in a distributed team
    is complicated and error prone and it erases the historic log
    of work done on the project.

- Commits don't contain real patches, but only snapshots of the file
    before and after the commit.
    This can lead to diffs which do not faithfully represent the semantics
    of a change.
    E.g. in following patch the diff algorithm assumes that the new
    code was spliced into the `initialize` function, although it's clear
    for a developer that in fact
    it was appended *after* the initialize function.

    ```rb
     class Person
       def initialize(name)
         @name = name
    +  end
    +
    +  def inspect
    +    @name
       end
     end
    ```

    Also, the number of context lines might not be optimal
    for code comprehension, as a deep understanding of the code
    is necessary to asses how many lines are necessary for good understanding.

    While alternative diffing algorithms can yield better results, there will
    always be edge cases where only the original developer knows the intended
    semantics of the change.

- Commits can't contain different changes for the same hunk.
    E.g. adding an item to a list might make it necessary to reformat
    the whole list.
    While those are semantically 2 different operations,
    only the combined changes are captured in a commit diff.


As a better suited system for literate patching
I'm proposing the literate patch project format.
A literate patch project consists of a sequence of patches (aka `.diff` files)
in the unified diff format.
Each patch denotes a new version of the project and contains
all necessary changes since the previous version.
This is reflected in the file names, which are
`<version>-<parent>.diff`

```sh
.
├── 0_1.diff
├── 0_2--0_1.diff
├── 0_2_1--0_2.diff
├── 0_3--0_2.diff
├── 0_4--0_3.diff
└── 0_5--0_4.diff
```


[uku-post]: /ukulele-fingering-chart-cli-tool-in-haskell
[CWEB]: https://www-cs-faculty.stanford.edu/~knuth/cweb.html
[Literate Programming]: https://www-cs-faculty.stanford.edu/~knuth/lp.html
