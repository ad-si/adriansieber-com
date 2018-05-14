---
title: Haskell - The Best Programming Language You Have Never Heard Of
...

The appeal to use a certain programming language
often correlates with it's popularity.
More popular means:

  - Easy to get started
  - A bigger ecosystem
  - More eyes to find bugs
  - More companies backing the language
  - More documentation on the Internet
  - Higher likelihood of future support
  - If it wasn't any good it wouldn't have become so popular

Or at least so the narrative goes.
Lately I've become a little wary of this story.
Maybe it can just as well mean:

  - Easy to get started, but not easy to maintain
  - A big ecosystem of low quality
  - More people need to agree on how to move forward
  - More companies have to use the language in order to appear "modern"
  - Hard to find high quality documentation
  - Susceptible to superficial trends
  - Popular because of an unfair monopoly (e.g. JavaScript in the Browser)

With this questions in mind I wanted to make an unbiased decision about
my language choices and therefore set out to try as many
different programming languages as possible.

The minutiae of this journey might be the content of another article,
but for now I just want to share my conclusion with you and highlight
the features which made the difference for me.

As it turns out, popularity wouldn't have been a good indicator
of Haskell's appeal.
It only ranks on position 22 on the
PYPL (PopularitY of Programming Language) index of May 2018,
position 25 on IEEE Spectrum's "The Top Programming Languages 2017" ranking
and position 48 on the Tiobe index of May 2018.

So what characteristics make up Haskell's appeal then?


# Static Types with Global Type Inference

Let's jump to the most important one right away:
Every single expression in Haskell is statically (i.e. at compile time) typed.

If you're now thinking of a heavy weight type system like in Java or C++
and are already disgusted, I have good news:
Haskell's syntax is very lightweight, more akin to Python,
as all types are globally inferred
(i.e. across function and file boundaries).

This means one doesn't have to explicitly state the type if the compiler
can figure it out on it's own (which it can in most cases).
E.g. a tuple like `nameAndIsMember = ("John Doe", True)` is
of type `(String, Bool)`.
No need to annotate it.
In practice this means you can write thousands lines of Haskell code
without annotating a single type, and yet your whole program is type safe.

Let's have a look at a little example:

> Examples in this article try to use equivalent language constructs
> for better comparability even though there might be more idiomatic
> versions in each language.

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

We define a union type for different shirt sizes and
and print a sentence according to John's size.

But what happens if we later decide to introduce another size `Huge`
and update the first line to be
`data ShirtSize = Small | Medium | Large | Huge`

```txt
shirt-size.hs:6:13: warning: [-Wincomplete-patterns]
    Pattern match(es) are non-exhaustive
    In a case alternative: Patterns not matched: Huge
  |
6 |   putStrLn (case johnsSize of
  |             ^^^^^^^^^^^^^^^^^...
```

The GHC (Glasgow Haskell Compiler - Haskell's default compiler) immediately
warns us that we forgot to handle the case that `johnsSize` might
be set to `Huge`. Awesome!

Now imagine writing the same code in Python, JavaScript,
or pretty much any other language.
If it was dynamic, it would have crashed at runtime if there was a person
with a `Huge` shirt size.
Or even worse, it would just ignore it silently.
That's what happens in Java with an `enum`,
a `switch` statement and no `default` case.

How would you even model the `ShirtSize` type in dynamic languages?
With strings?
Then who would save you from typos?

```txt
shirt-size.hs:3:13: error:
    • Data constructor not in scope: Medum
    • Perhaps you meant ‘Medium’ (line 1)
  |
3 | johnsSize = Medum
  |             ^^^^^
```

Even if you can find a workaround (Python supports enums since 3.4) it will
hardly be as elegant and straight forward as in Haskell.
It's expressive type system in combination with the powerful compiler
make data modeling one of Haskell's core strengths.


# Pure Functions

All functions in Haskell are pure.
This means that the function's return value
is only dependent on its input arguments
and that the function can not perform any side effects
like changing an external state or doing IO.

Here is a short example why this is desirable:

```javascript
// Reverses and capitalizes a string
function reverseAndShout (string) {
  const reversed = string.split('').reverse().join('')
  console.info(`Reversed: ${reversed}`)
  const shouted = reversed.toUpperCase()
  dataBase.write(shouted)
  lastShouted = shouted
  return shouted
}
```

```haskell
reverseAndShout :: Text -> Text
reverseAndShout text =
  let
    reversed = reverse text
    shouted = toUpper reversed
  in
    shouted
```

The JavaScript function sounds innocently enough.
It takes a string, reverses and capitalizes it.
What could go wrong?
This might be the kind of function you import from a 3rd party library
or maybe a colleague wrote it.

However, if you look under the hood it does all kind of things.
It logs to the terminal, it writes to a database and it overwrites global state
although it was only supposed to transform the string.
Also bare in mind that every single one of those actions
could potentially cause an error and crash your program.

On the other hand, the Haskell function is defined to receive `Text`,
transform it in some way and return `Text`.
And that's really all the function can do.
You couldn't even make it print something to the terminal if you wanted to.
The type signature `Text -> Text` clearly states what the function can do
and the compiler won't compile it if you try to do something else.

On a side note:

The type signature line `reverseAndShout :: Text -> Text` is optional,
but it's considered good practice to add it to functions
which shall be exported, as it acts as documentation for the function.

It's also helpful for development as it tell the compiler what your
function is supposed to do and the compiler will warn you accordingly
if you're actually implementing something else.

Also:
The idiomatic version of the function in Haskell would actually be
`reverseAndShout = toUpper . reverse`, but that's a topic
for another post 😉.
Feel free to search for "pointfree style" if you want to dig deeper.


# Strong Immutability

Another hallmark feature of Haskell is its guarantee that
values can not be mutated (changed).

Let's look at an example again to clarify the advantages:

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
setTimeout(() => console.log(reverseAndShoutName(john)), 5)
```

and the output of the program when running it with `node` is:

```shell
$ node reverseAndShoutName.js
EODNHOJ
JohnDoe
HTIMSDENIFEDNU
```

Wow, this feels pretty dangerous.
Again, I was using the function `reverseAndShoutName`
with the expectation that it generates the fullName and returns it reversed
and capitalized.
What it did, however, was to additionally add a `fullName` field to the
object, which it never was supposed to have, and even worse it randomly
deleted and changed a field after some time.

This means whenever you pass an object to a function in a mutable language,
you have absolutely no idea or guarantees
what will happen to the object during and after the function call
without carefully checking the complete code of the function.

A workaround would be to always clone an object before passing it
to a function.
Funny / sad thing is:
JavaScript doesn't even have a native way to deep clone an object.

Haskell on the other hand:

```haskell
data Person = Person {firstName :: Text, lastName :: Text}

reverseAndShoutName :: Person -> Text
reverseAndShoutName person =
  let fullName = firstName person <> lastName person
  in fullName & reverse & toUpper

john :: Person
john = Person "John" "Doe"

main :: IO ()
main =
  putStrLn (reverseAndShoutName john)
```

Maybe this feels like I'm just not implementing the same code as in JavaScript,
but that's exactly the point:
Within the constraints of my types `data Person = …`,
`reverseAndShoutName :: Person -> Text`, `john :: Person` and `main :: IO ()`
there is really not much I can do to break the code.

There is no way to change the the local `person` constant in the
`reverseAndShoutName` function,
there is no way to run an async timeout somewhere,
and there is no way to log anything outside of the `main` function,
which is explicitly typed as returning an `IO` computation resolving
to the Unit value `()` (meaning no interesting value).

But what if you really want to use a changed version of John?
You make a copy with a changed field!

```haskell
john = Person "John" "Doe"
john2 = john {lastName = "Smith"}
```

And don't worry about performance.
The GHC (Glasgow Haskell Compiler - Haskell's standard compiler)
heavily optimizes such scenarios by reusing existing elements.


# Lazy Evaluation

This is a feature most unique to Haskell, as it is besides its predecessor
"Miranda" the only major language which is per default lazy evaluated.

So what exactly does it mean?
Let's have a look at following artificial Python code:

```python
valueA = expensiveComputation()
valueB = anotherExpensiveComputation()

if valueToPrint == 'A':
  print(valueA)
else:
  print(valueB)
```

This is really badly implemented,
because no matter what value is actually supposed to be printed,
it always calculates both of them.
It eagerly evaluates the code as soon as a line of code is executed.

In Haskell on the other hand, it merely registers in the first 2 lines
how `valueA` and `valueB` can be computed and only starts to evaluate the code
as soon as the value is to be printed.
It evaluated the code lazily.

```haskell
valueA = expensiveComputation
valueB = anotherExpensiveComputation

main =
  putStrLn (if valueToPrint == "A"
    then valueA
    else valueB)
```

While it is easy to recognize the problem
and fix the Python code in this situation by pulling the value
assignments into the `if … else` statement,
it becomes harder and harder to notice such missteps in a larger code base.
Haskell again just does the right thing per default

Another cool side effect of lazy evaluation is infinite lists:

```haskell
allNumbers = [1..]
allNumbersDoubled = allNumbers & map (*2)

main =
  allNumbersDoubled & take 5 & print
```

This prints `[2, 4, 6, 8, 10]` and there is no other
programming language where this can be written as concisely and beautifully.

(And don't tell me be about Python's list comprehension.
This was actually invented by Haskell and is still supported,
but considered bad practice as a few simple functions
can achieve the same thing more readable and without the overhead
of introducing another syntax construct.)

---

These were the 4 corner stones which make Haskell a great language.
They alone would make Haskell worthwhile, but it turns out even
in other parts which aren't unique to Haskell it has a pretty strong standing.
To not make the post longer than it already is I'm going to sum them
up in a few short sentences.
If you want to dig deeper there are several resources listed at the end
where to go next.


# The Rest

## REPL

GHC provides one of the best REPLs of any programming language.
Some Lisp dialects have similar features, but none of the C like languages.

Because of Haskell's purity functions don't interact with a global state
and are therefore perfectly suited for being tweaked and tested in the REPL.


## Unobtrusive Syntax

As you have noticed by now, the syntax is really boiled down to the minimum.
This makes Haskell a prime candidate for writing EDSLs
(Embedded Domain Specific Language) and there exist tons of them.
The motivation is to generate a non type safe language like e.g. HTML or CSS
with a type safe language, while keeping the looks of the original language.

E.g. https://github.com/hadolint/language-docker lets you write
Dockerfiles in Haskell which look just like normal Dockerfiles,
except now they are type safe and it's harder to make mistakes.

```haskell
import Language.Docker

main = putStr $ toDockerfileStr $ do
    from "node"
    run "apt-get update"
    runArgs ["apt-get", "install", "something"]
```

It's also pretty interesting, that many code fragments which look like
first class syntax constructs are actually just normal functions
which are defined in Haskell's standard library called "Prelude"

e.g. the `&` I used before is actually just a function with the signature
`(&) :: a -> (a -> b) -> b`, meaning it takes a value of some type `a`,
and a function of type `a -> b`, applies the function to the value and returns
the result of type `b`.
And voila: You have a function which reverses the application order.


# Awesome Ecosystem

- Hackage
- Stackage

# Great Community

- Slack
- IRC
- Reddit

# Great Documentation

# Concurrency

# Haskell-Like Languages

# Reliable / Maintainable / Refactorable / Testability

- High consistency in output / time


Not talked about: performance

Not so good: Legacy problems

Resources:

- https://github.com/Gabriel439/post-rfc/blob/master/sotu.md

