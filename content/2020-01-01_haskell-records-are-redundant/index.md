+++
title = "Haskell records are redundant"
draft = true

[taxonomies]
tags = ["Haskell"]
+++

```hs
person = Person
  { name = "John Doe"
  , color = "green"
  , height =  "172"
  , job = "Teacher"
  }
```

vs

```haskell
person = Person
  (Name "John Doe")
  (Color "green")
  (Height "172")
  (Job "Teacher")
```


```haskell
newtype Name = Name String deriving (Show)
newtype Color = Color String deriving (Show)
newtype Height = Height String deriving (Show)
newtype Job = Job String deriving (Show)

data Person = Person Name Color Height Job deriving (Show)

person = Person
  (Name "John Doe")
  (Color "green")
  (Height "172")
  (Job "Teacher")

main = do
  putStrLn $ show person
  putStrLn $ case person of
    Person (Name "Joe") _ _ _ -> "yes"
    _ -> "no"
```

- http://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extensions-to-the-record-system



adius [3:14 PM]
I was just wondering why I should use records at all. They just fell like a crutch in Haskell.
E.g. an easy way to avoid them would be this pattern:

```person = Person
  { name = "John Doe"
  , color = "green"
  , height =  "172"
  , job = "Teacher"
  }```

vs

```person = Person
  (Name "John Doe")
  (Color "green")
  (Height "172")
  (Job "Teacher")```

What are your thoughts on this? Am I missing something?

lyxia [3:20 PM]
they have nice syntax for record creation/update
where you don't need to worry about the order of fields
also pattern matching,  `case ... of (Person{name = "Joe"}) -> ... ; _ -> ...`

adius [3:22 PM]
> worry about the order of fields
I'd actually say this is a disadvantage. It's more readable if it always has the same order of fields ^^

lyxia [3:23 PM]
fair point
Admittedly these are fairly minor issues, that are largely addressed by lenses+generics with the style you propose.

adius [3:25 PM]
```case person of
    Person (Name "Joe") _ _ _ -> "yes"
    _ -> "no"```
doesn't look to bad either

lyxia [3:26 PM]
But your code breaks if you add or remove or move a field
this has happened before with TH for example

adius [3:27 PM]
But I'll get a good compiler warning `The constructor 'Person' should have 3 arguments, but has been given 4`
So it's an easy fix
A partial record is harder to track

lyxia [3:30 PM]
you also get a warning for that with `-Wall`, that everyone uses.

adius [3:31 PM]
yeah that's true
I guess probably the biggest drawback is that it's harder to make the code more type safe with the second approach. `Job Teacher` is way more complicated (and unintuitive) to implement, than `job = Teacher` (edited)

lyxia [3:37 PM]
How is it more complicated?
BTW newtype-fields may also be one way to make records extensible https://www.reddit.com/r/haskell/comments/8c79cl/combining_multiple_readert_or_statet_with_record/
reddit
Combining multiple ReaderT or StateT with record types without labels • r/haskell
I've always thought that field labels are unnecessary for many uses cases of extensible records, since we can just use the type of the field...

adius [3:40 PM]
Ok, complicated is the wrong term. It's just not very clean:

```data JobType = Teacher | Doctor deriving (Show)
newtype Job = Job JobType deriving (Show)```

lyxia [3:41 PM]
This just popped on reddit, and seems related (newtypes for named function parameters) https://www.reddit.com/r/haskell/comments/8e3vf3/package_worth_highlighting_named_adds_support_for/
reddit
Package worth highlighting: "Named". Adds support for named parameters, keyword arguments to Haskell. Better than the other approaches I know of, e.g. Records, Default typeclass. It even supports automatic currying. • r/haskell
1 points and 0 comments so far on reddit
yeah it would be nice to somehow avoid the `Job` declaration.

adius [4:02 PM]
Thanks for the link. Named looks quite interesting.

bitemyapp [4:07 PM]
what
use recordwildcards
decouples product arity from use site
