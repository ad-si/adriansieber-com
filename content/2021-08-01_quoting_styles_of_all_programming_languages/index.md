+++
title = "Quoting Styles Comparison Of Major Programming Languages"
author = "Adrian Sieber (ad-si.com)"
draft = true

[taxonomies]
tags = ["programming", "language", "style"]
+++


Language    |    `"`     |    `'`     |  `` ` ``   |    `"""`   | Extras
------------|------------|------------|------------|------------|---------------
Python      | String     | String     |            | Multi-line | `f" … "`, …
Java        | String     | Character  | Multi-line |            |
JavaScript  | String     | String     | Multi-line |            | ``x` … ` ``
TypeScript  | String     | String     | Multi-line |            | ``x` … ` ``
C#          | String     | Character  |            |            | `@" … "`
C           | String     | Character  |            |            |
C++         | String     | Character  |            |            | `R" … "`
PHP         | Escaped    | Unescaped  | Execution  |            | `<<<X … X`
R           | Multi-line | Multi-line |            |            |
Objective-C | String+    | Character+ |            |            |
Swift       | String     |            |            | Multi-line | `#" … "`
Matlab      |            | String     |            |            |
Kotlin      | String     | Character  |            | Multi-line |
Go          | String     | Character  | Multi-line |            |
Rust        | String     | Character  |            |            | `r#" … "`
Ruby        | String     | String     | Execution  |            | `<<~X … X`
Ada         | String     | Character  |            |            |
Scala       | String     | Character  |            | Multi-line | `x" … "`
Dart        | String     | String     |            | Multi-line | `''' … '''`
Lua         | String     | String     |            |            | `[[ … ]]`
Julia       | String     | Character  |            | Multi-line | `raw" … "`
Groovy      | String     | String     |            | Multi-line |
Perl        | Escaped^   | Unescaped^ |            |            | `<<<X … X`
Pascal      |            | String     |            |            |
Haskell     | Char List  | Character  |  Infix*    |            |
TypeScript  | String     | String     | Multi-line |            |
PureScript  | String     | Character  |            | Multi-line |
Elm         | String     | Character  |            | Multi-line |
Elixir      | Multi-line | Char List  |            | Multi-line | `~S"""…"""`
Erlang      | Char List  |            |            |            |
Clojure     | String     | Lit. List  |            |            |
Fortran     | String     | Character  |            |            |
OCaml       | Multi-line | Character  |            |            |


`+`: Actually needs to be `@" … "` and `@' … '`

`^`: Both support multi-line strings

`*`: Turns a function into an infix operator (e.g. `add 1 2` -> ``1 `add` 2`` )

`x`: Stands for several different keywords which are allowed at this position
