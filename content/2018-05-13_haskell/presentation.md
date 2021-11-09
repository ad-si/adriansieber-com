---
title: Haskell and the case for purely functional programming
author: Adrian Sieber
date: 2021-06-16
theme: default
colortheme: owl
innertheme: circles
aspectratio: 169
image: ./expanding_brain.jpg
draft: true
---

# Haskell Logo

![](./logo.png)

---

\Large

- Large number of programming languages have been invented over the years
- Only a small number of popular languages make up the majority
of the code written today

**Why? Are they better than the rest?**

---

\Large

Developers often choose more popular languages,
because more popular means:

  - Easy to get started
  - A bigger ecosystem
  - More eyes to find bugs
  - More companies backing the language
  - More documentation on the Internet
  - Higher likelihood of future support
  - If it wasn't any good it wouldn't have become so popular

---

\Large

But maybe it can just as well mean:

  - Easy to get started, but not easy to maintain
  - A big ecosystem of low quality
  - Companies pushed the language for self promotion
  - More companies have to use the language in order to appear "modern"
  - Hard to find high quality documentation
    (instead of simple "How to get started" tutorials)
  - Susceptible to superficial trends
  - Popular because of an unfair monopoly
    (e.g. JavaScript in the Browser)


---

\Large

**Popularity isn't a good indicator of Haskell's appeal**

- Position 22 on the PYPL (PopularitY of Programming Language) index (May 2018)
- Position 25 on IEEE Spectrum's "The Top Programming Languages 2017"
- Position 48 on the Tiobe index (May 2018).

**but**

> The functional language Haskell is the tag most visited outside of the workday [^1]

[^1]: https://stackoverflow.blog/2017/04/19/programming-languages-used-late-night/


---

\center \Huge

**Developers love to spend their free time coding in Haskell!**


---

\Large

## History of FP Languages

- 1920 - **Combinatory Logic** by Moses Schönfinkel and Haskell Curry

  > A combinator is a higher-order function
  > that uses only function application
  > and earlier defined combinators to define a result from its arguments.

- 1930 - **(Typed) Lambda Calculus** by Alonzo Church
- 1936 - **Turing Machine** by Alan Turing


---

\Large

- 1950 - **Lisp** - First functional programming language (but not pure!)
- 1973 - **ML** (Meta Language) - Uses Polymorphic Hindley–Milner type system
  - 1987 - **Caml** - Dialect of ML
  - 1996 - **OCaml** - Extends the Caml with object-oriented features
- 1985 - **Miranda** - Lazy & purely functional language, but proprietary


---

\Large

1990 - **Haskell** - A committee consolidated existing functional languages
    to serve as a basis for future research [^1]

- 2007 - **Idris** - Dependent types (list with x entries), totality checker
- 2012 - **Elm** - DSL for building webapps
- 2015 - **PureScript** - Compiles to general purpose JavaScript

[^1]: Only major language invented by committee!

---

\center \Huge

**What makes Haskell so great then?**

---

\Large

## Static Types with **Global** Type Inference

Every single expression in Haskell is statically (i.e. at compile time) typed.

&nbsp;

Don't think "Java" of "C++" => More like Python + unobtrusive Types


---

\Large

Types are **globally** inferred
(i.e. across function and file boundaries).

=> Explicitly stating type not necessary

=> Write thousands of lines of code without a single type signature

---

\Large

Let's try it out in the Haskell REPL:

&nbsp;

```haskell
> nameAndIsMemberTuple = ("John Doe", True)
> :t nameAndIsMemberTuple
nameAndIsMemberTuple :: ([Char], Bool)
```

&nbsp;

Annotations not necessary!

---

\Large

## Disclaimer

&nbsp;

> Examples in this article try to use equivalent language constructs
> for better comparability even though there might be more idiomatic
> versions in each language.

---

\Large

```haskell
data ShirtSize = Small | Medium | Large

johnsSize = Medium

main =
  putStrLn (case johnsSize of
      Small -> "Eat more spinach!"
      Medium -> "You're just average."
      Large -> "Is the air thinner up there?"
    )
```

---

\Large

```haskell
data ShirtSize = Small | Medium | Large
```

- FP speak: Algebraic data type (sum type)
- Otherwise: Union type
- Create new types for every- and anything!

---

\Large

What happens if we later decide to add size `Huge`
and update the first line to be:

```haskell
data ShirtSize = Small | Medium | Large | Huge
```

---

\Large

```txt
shirt-size.hs:6:13: warning: [-Wincomplete-patterns]
    Pattern match(es) are non-exhaustive
    In a case alternative: Patterns not matched: Huge
  |
6 |   putStrLn (case johnsSize of
  |             ^^^^^^^^^^^^^^^^^...
```

&nbsp;

GHC (Glasgow Haskell Compiler - the default compiler) warns us!

---

\Large

In other languages:

- Would have crashed at runtime if there was a person
    with a `Huge` shirt size.

- Would just ignore it silently
    E.g. Java with an `enum`,
    a `switch` statement, and no `default` case.

---

\Huge

How would you even model the `ShirtSize` type in dynamic languages?

&nbsp;

With strings?

&nbsp;

Who would save you from typos?

---

\Large

In Haskell on the other hand:

```txt
shirt-size.hs:3:13: error:
    • Data constructor not in scope: Medum
    • Perhaps you meant ‘Medium’ (line 1)
  |
3 | johnsSize = Medum
  |             ^^^^^
```

---

\Large

Even if you find a workaround (Python supports enums since 3.4) it will
hardly be as elegant and straight forward as in Haskell.

&nbsp;

Expressive type system + powerful compiler
=> data modeling is one of Haskell's core strengths

---

\Large

##  Pure Functions

&nbsp;

All functions in Haskell are pure.

- Return value depends only on its input arguments
- Function can not perform any side effects (if not explicitly stated)

---

\Large

Short example why this is desirable:

You see following innocent function in a big code base.
**What does it do?**

```javascript
const alertText = reverseAndShout('JS is the best!')
```

---

\Large

```javascript
// Reverses and capitalizes a string
// (and is evil)
function reverseAndShout (string) {
  const reversed = string.split('').reverse().join('')
  console.info(`Reversed: ${reversed}`)  // IO
  const shouted = reversed.toUpperCase()
  dataBase.write(shouted)  // Might cause an error
  lastShouted = shouted  // Mutating global state
  return shouted
}
```

---

\Large

## Haskell

&nbsp;

```haskell
reverseAndShout :: Text -> Text
reverseAndShout text =
  let
    reversed = reverse text
    shouted = toUpper reversed
  in
    shouted
```

---

\Large

Want to print to terminal?

```haskell
reverseAndShout :: Text -> IO Text
```

&nbsp;

Want to change global state?

```haskell
reverseAndShout ::
  (GlobalState, Text) -> (GlobalState, Text)
```

---

\Large

## Sidenote

&nbsp;

Type signature `reverseAndShout :: Text -> Text` is optional,
but considered good practice, as it acts as documentation
and improves error messages during development.

&nbsp;

Tell the compiler what your
function is supposed to do and it will help you implement it!

---

\Large

## Sidenote 2

&nbsp;

The idiomatic version of the function would actually be:

```haskell
reverseAndShout = toUpper . reverse
```

&nbsp;

This is called "pointfree style"

---

\Huge

## Strong Immutability

&nbsp;

Guarantee that values can not be mutated (changed).

---

```javascript
function reverseAndShoutName (person) {
  person.fullName = person.firstName + person.lastName
  setTimeout(
    () => {delete person.firstName; person.lastName = 'Smith'},
    1
  )
  return person.fullName
    .split('').reverse().join('')
    .toUpperCase()
}

const john = {firstName: 'John', lastName: 'Doe'}

console.log(reverseAndShoutName(john))
console.log(john.fullName)
setTimeout(() => console.log(john), 5)
```

---

\Large

Output of the program when running with `node`:

&nbsp;

```shell
$ node reverseAndShoutName.js
EODNHOJ
JohnDoe
{ lastName: 'Smith', fullName: 'JohnDoe' }
```

---

\huge

You have absolutely **no idea or guarantees**
what will happen to a object during and after a function call
**without carefully checking the complete code** of the function!

---

\Large

```haskell
data Person = Person
  {firstName :: Text, lastName :: Text}

reverseAndShoutName :: Person -> Text
reverseAndShoutName person =
  let fullName = firstName person <> lastName person
  in fullName & reverse & toUpper

john :: Person
john = Person "John" "Doe"

main :: IO ()
main = putStrLn (reverseAndShoutName john)
```

---

\Large

Maybe it feels like I'm just not implementing the same code?

**That's the point!**

With

- `data Person = …`
- `reverseAndShoutName :: Person -> Text`
- `john :: Person`
- `main :: IO ()`

there is really not much I can do to break the code.

---

\Large

What if you want to use a changed version of John?

You make a copy with a changed field!

```haskell
john = Person "John" "Doe"
john2 = john {lastName = "Smith"}
```

And don't worry about performance:<br>
GHC heavily optimizes such scenarios by reusing existing elements.

---

\Large

## Lazy Evaluation

- Most unique feature of Haskell
- Only major language (besides "Miranda") which is lazy per default

---

\Large

Some artificial Python code:

&nbsp;

```python
valueA = expensiveComputation()
valueB = anotherExpensiveComputation()

if valueToPrint == 'A':
  print(valueA)
else:
  print(valueB)
```

---

\Large

Badly implemented:

- Whatever value is supposed to be printed => calculates both of them
- Python eagerly evaluates the code as soon as a line of code is executed

---

\Large

## Haskell

&nbsp;

- GHC registers how `valueA` and `valueB` can be computed
- Starts to evaluate the code as soon as the value is needed
- It evaluated the code lazily

```haskell
valueA = expensiveComputation
valueB = anotherExpensiveComputation

main =
  putStrLn (if valueToPrint == "A"
    then valueA
    else valueB)
```

---

\Huge

It becomes harder and harder to notice such missteps in a larger code base.

&nbsp;

Haskell does the right thing!

---

\Large

Cool side effect of lazy evaluation: **Infinite lists**

&nbsp;

```haskell
allNumbers = [1..]
allNumbersDoubled = allNumbers & map (*2)

main =
  allNumbersDoubled
  & take 5
  & print

-- Prints [2, 4, 6, 8, 10]
```

---

\Huge

> There is no other programming language
> where this can be written as concisely and beautifully.

&nbsp;

\small
And don't tell me be about Python's list comprehension.
This was actually invented by Haskell and is still supported,
but considered bad practice.
A few simple functions can achieve the same thing
more readable and without the overhead
of introducing another syntax construct.

---

\Large


## The Rest

&nbsp;

- Best REPL (maybe except for some Lisps)
  - Functions don't interact with a global state
    => perfectly suited for being tweaked and tested in the REPL
- Unobtrusive Syntax
  - Prime candidate for writing EDSLs
    (generate a non type safe language like e.g. HTML or CSS in a safe way,
    while keeping the looks of the original language.)

---

\Large

github.com/hadolint/language-docker lets you write
Dockerfiles in Haskell, which look like normal Dockerfiles,
but now they are type safe and it's harder to make mistakes.

&nbsp;

```haskell
import Language.Docker

main = putStr $ toDockerfileStr $ do
  from "node"
  run "apt-get update"
  runArgs ["apt-get", "install", "something"]
```

---

\Large

- Many syntax constructs are actually just normal functions
- Defined in the standard library "Prelude"

e.g. the `&` I used before is actually just a function with the signature:

```haskell
(&) :: a -> (a -> b) -> b
```

- Takes a value of some type `a`
- Applies a function to the value
- Returns the result of type `b`

&nbsp;

**And voila: A function which reverses the application order**

---

\Large

- Awesome ecosystem of packages on stackage.org
- Excellent quality of 3rd party tools<br>
    => Haskell developers care a lot about writing
      correct, stable, and secure software
- Good documentation through default documentation generator "Haddock"
- Awesome community
- Considered "Best in class" for parallelism and concurrency

---

\Large

## Maintainability and Refactorability

Quote from Gabriel Gonzalez:

&nbsp;

> Haskell is unbelievably awesome for maintaining large projects.
> There's nothing that I can say that will fully convey
> how nice it is to modify existing Haskell code.
> You can only appreciate this through experience.

&nbsp;

The compiler is guiding one through any
refactoring based on the types.

---

\Large

## The Not so Good

&nbsp;

- `[Char]` vs `String` vs `Text` vs `ByteString` vs …
- Laziness can apparently cause excessive memory usage
- No packages for niche software
- Cold compilation times

---

\Large

## Example Apps - Most Starred Projects on GitHub

&nbsp;

- **ShellCheck** - Static analysis tool for shell scripts
- **Pandoc** - Universal document converter
- **Hasura** - Blazing fast, instant realtime GraphQL APIs on Postgres
- **PostgREST** - Automatic REST API for any Postgres database
- **github/semantic** - Parsing, analyzing, and comparing
      source code for many languages
- **PureScript** - Strongly-typed language that compiles to JavaScript.
- **Elm** - Strongly-typed language that compiles to JavaScript.
- **facebook/Haxl** - Simplify access to remote data
      (E.g. databases and web-based services)

---

\Huge

## Try It Out

&nbsp;

- [tryhaskell.org](https://tryhaskell.org)
- [repl.it](https://repl.it)



