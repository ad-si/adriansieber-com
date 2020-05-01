+++
title = "Optional Function Names for Haskell"
draft = true

[taxonomies]
tags = ["Haskell"]
+++

I was looking for a language extension like this:

```haskell
{-# LANGUAGE OptionalFunctionName #-}

hello :: String
= "Hello World!"

helloName :: String -> String
= \name -> "Hello " ++ name ++ "!"

helloFull :: String -> String -> String
= \first second ->
  "Hello " ++ first ++ " " ++ second ++ "!"
```

Does it exist?
