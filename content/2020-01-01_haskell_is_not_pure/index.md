+++
title = "Haskell Is Not Pure"
draft = true
+++

Here is a simple function which builds a hello message from a name:

```haskell
showHi :: String -> String
showHi name =
  unwords ["Hi", name, "how", "is", "it", "going?"]
```

When we run it with `showHi "John"`
it yields `"Hi John how is it going?"`.

And just as we love it in Haskell, the function is pure and therefore
the return value is the same for same arguments:
Every call of `showHi "John"` will yield `"Hi John how is it going?"`

Or will it?

```haskell
unwords :: [String] -> String
unwords _ =
  "I WILL NOT GREET YOU"


showHi :: String -> String
showHi name =
  unwords ["Hi", name, "how", "is", "it", "going?"]
```

We didn't change the arguments, yet the return value changed.
Seems like Haskell functions aren't that pure after all.


---


```txt
test.hs:4:3: error:
    Variable not in scope: putStrLn :: [GHC.Types.Char] -> t
  |
4 |   putStrLn "`putStrLn` is loaded from a global state"
  |   ^^^^^^^^
```


```haskell
{-# LANGUAGE NoImplicitPrelude #-}

import Prelude as Prelude

main =
  putStrLn "`putStrLn` is loaded from a global state"
```


Instead of

```haskell
{-# LANGUAGE NoImplicitPrelude #-}

import Prelude (Char, IO)
import qualified Prelude


sayHi :: [Char] -> [Char]
sayHi name =
  Prelude.unwords ["Hi", name, ",how", "is", "it", "going?"]


main :: IO ()
main =
  Prelude.putStrLn (sayHi "John")
```

we should write

```haskell
{-# LANGUAGE NoImplicitPrelude #-}

import Prelude (Char, IO)
import qualified Prelude


sayHi :: Foldable t => (t a -> a) -> a -> a
sayHi unwords name =
  unwords ["Hi", name, ",how", "is", "it", "going?"]


main :: IO ()
main =
  Prelude.putStrLn (sayHi Prelude.unwords "John")
```




Or develop a new programming language where all external arguments
are labeled as such:


```haskell
{-# LANGUAGE NoImplicitPrelude #-}

import Prelude (Char, IO)


sayHi :: ([[Char]] -> a) -> [Char] -> a
sayHi #unwords name =
  unwords ["Hi", name, ",how", "is", "it", "going?"]


main :: IO ()
main =
  Prelude.putStrLn (sayHi "John")
```

Should be possible to write signatures in a polymorphic way
=> Independent of imported version of classes

```haskell
newtype Test = Test Int

joinParts :: [a] -> a
joinParts _ = Test 1
```

