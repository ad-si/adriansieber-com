#import "@preview/touying:0.4.2": *

#set text(font: "Arial")

#let greenify(txt) = text(fill: rgb(20%, 50%, 30%), txt)

#let quoteBlock(content) = block(stroke: (left: 0.1em), inset: 1em, content)

#let s = themes.simple.register()
#let (init, slides) = utils.methods(s)
#show: init

#let (slide, empty-slide) = utils.slides(s)
#show: slides

// TODO: https://www.youtube.com/watch?v=79PSagCD_AY
// TODO: https://www.talentopia.com/news/the-rust-programming-language-its-history-and-why/


#slide[
  == Hi there! 👋

  I'm *Adrian Sieber*. \
  I work at *Feram*. \
  More Information at *adriansieber.com*.
]

#slide[
  #set align(center)
  = History and Future of Rust
  #v(1em)
  #image("time-traveling-crab.jpeg", height: 70%)
]

#slide[
  = Who of you used …
  #v(1em)
  - C: 6/12
  - Go: 6/12
  - OCaml: 0/12
  - Elm: 1/12
  - Haskell: 4/12
  - PureScript: 0/12
  - Rust: 11/12
]


#slide[
  == Short History

  #set terms(
    separator: h(1.5em),
    hanging-indent: 4em,
    spacing: 2em
  )
  / #greenify("2006"): Graydon Hoare created Rust as a personal project
          while working at Mozilla.

  // Graydon's website:
  // https://graydon2.dreamwidth.org/
  // https://github.com/graydon

  / #greenify("2010"): Rust was published

]


#slide[
  / #greenify("2010-06-16"): First public commit

  / #greenify("2010-06-24"): Adding most of the source files \
      #underline[https://github.com/rust-lang/rust/commit/d6b7c96c3eb]

      - Rust was originally written in *OCaml*
      - *Brendan Eich* -- the creator of JavaScript --
          is listed as an author

  // TODO: https://brson.github.io/2021/05/02/rusts-most-unrecognized-contributor
]


#slide[
  // http://venge.net/graydon/talks/intro-talk-2.pdf
  / #greenify("2010-07"): Mozilla Annual Summit - Project Servo
      #quoteBlock[
       - Rust is a language that mostly cribs from past languages. Nothing new.
       - The syntax is, really, about the last concern.
       - That was just a “taste” so you don't get all
          frustrated wondering what it looks like and/or
          assume that at the last minute it's going to read
          like Lisp or Haskell -
          (Hush, I know and love these languages,
          but there is a time and place).
      ]

  / #greenify("2015-05-14"): *Rust 1.0* was released (first stable version)
]


#slide[
  #set align(center)
  == How Exactly Was it Influenced?

  #image("family-tree.svg", height: 80%)
]

#slide[
  - *Meta Language Family*
    - *Standard ML*, *OCaml*, *Haskell*:
      - Algebraic data types (-> Enums)
      - Pattern matching
      - Type inference
    - *ML Kit*, *Cyclone*:
      - Region based memory management
    - *Haskell*:
      - Typeclasses (-> Traits)
      - Type families
    - *Elm*
      // https://blog.rust-lang.org/2016/08/10/Shape-of-errors-to-come.html
      #quoteBlock[
        … the updated `--explain` messages draw heavy inspiration
        from the Elm approach.
      ]
  - *C Family*
    - *C++*:
      - References
      - RAII (Resource acquisition is initialization)
      - Smart pointers
      - Move semantics
      - Monomorphization
      - Memory model
    - *Newsqueak*, *Alef*, *Limbo*:
      - Channels
      - Concurrency
]

#slide[
  - *Erlang*:
    - Message passing
    - Thread failure
  - *Swift*: Optional bindings
  - *Scheme*: Hygienic macros
  - *C\#*: Attributes
  - *Ruby*: Closure syntax
]

#slide[
  == Takeaway
  === 2 Main Influences: *ML Family* and *C Family*
  === Cherry picking from other languages
]


#slide[
  #set align(center)

  // Generated with:
  // https://www.classtools.net/Venn/202406-FastFunctionalMemorySafeQGQ2RB
  // Password: test

  #image("venn-diagram.png")

  // TODO: Use chetz-venn once it is available
  // #import "@preview/cetz:0.2.1"
  // #import "@preview/cetz-venn:0.1.0"
]


#slide[
  == Other Influences

  - npm, pip, bundler, … -> Cargo package manager
  - `go fmt` -> Rustfmt
  - Javadoc -> Rustdoc
  - …
]


// TODO: https://medium.com/@codilime/the-future-of-rust-characteristics-popularity-and-challenges-7de4db5ebd67


#slide[
  == The Future of Rust
]

#slide[
  // https://star-history.com/#apple/swift&golang/go&rust-lang/rust&Date
  #image("star-history.png")
]

#slide[
  // https://lib.rs/stats
  #image("number-teams.png")
  #image("daily-downloads.png")
]

#slide[
  == Roadmap
  // https://lang-team.rust-lang.org/roadmaps/roadmap-2024.html
  #image("roadmap.png", height: 80%)
]


#slide[
  … but also

  *Rust's speed and resource efficiency comes at a cost:*

  - Hard to learn and master
  - Development is slower
  - Thinking about often unimportant details \
    (lifetimes, borrowing, memory management, …)
]


#slide[
  // == Rust as a gateway drug to Haskell
  // http://xion.io/post/programming/rust-into-haskell.html

  #image("blog-screenshot.png")
]


#slide[
  == Some Areas are Still Immature

  github.com/UgurcanAkkok/AreWeRustYet

  #columns(2, gutter: 0.5em)[
    #image("arewerustyet.png", height: 60%) \
    - Game development
    - Machine Learning
    - GUI frameworks
    - Web development
    - Embedded systems
  ]
]


#slide[
  #set align(center)

  === Rust has been the most admired programming language for 8 years in a row

  #image("stackoverflow.png", height: 75%)
]


#slide[
  #set align(center)
  === FYI: This presentation was built with Typst
  ==== An open source LaTeX successor written in Rust

  // https://typst.app

  #image("typst.png", height: 70%)
]


#slide[
  == Thank you for your attention!

  I'm *Adrian Sieber*. \
  I work at *Feram*. \
  More Information at *adriansieber.com*.
]
