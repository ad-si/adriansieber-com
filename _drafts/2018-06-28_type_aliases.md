So `type` for atomic data is considered bad practice, because it offers a false sense of security.
E.g. in following code, nothing saves me from mixing up `FirstName` and `LastName` (as shown):

```
type FirstName = Text
type LastName = Text
type Message = Text

firstName :: FirstName
firstName = "John"

lastName :: LastName
lastName = "Doe"

greet :: FirstName -> LastName -> Message
greet firstName lastName =
  "Hi " <> firstName <> " " <> lastName

main :: IO ()
main =
  putStrLn $ greet lastName firstName
```

Which is good advice I guess as you could use newtypes in this case.

However, I really like the increased readability of `greet :: FirstName -> LastName -> Message` instead of `greet :: Text -> Text -> Text`.
Especially when it's rendered in Haddock, you don't see the paramater names anymore.


But sometimes there is no need for added type security, cause there is nothing to mix up:

```
greet :: Text -> Text
greet name = "Hi " <> name
```

yet, the function signature would still benefit from more descriptive names
So I was wondering if the Haskell community should / could agree on a pattern for this case. E.g.

```
greet :: NameText -> MessageText
greet name = "Hi " <> name
```
or

```
greet :: Name_Text -> Message_Text
greet name = "Hi " <> name
```
In comparison to a function

```
greet :: GreetWordText -> MessageText
greet greetWord = greetWord <> " John Doe"
```

Add screenshot of
http://hackage.haskell.org/package/blaze-svg-0.3.6.1/docs/Text-Blaze-Svg.html

What's the show for?
It's not even very useful, as all the parameters have to be of the same type
so the example they give is this
```
makeSimplePath :: S.AttributeValue
makeSimplePath =  mkPath do
  l 2 3
  m 4 5
```

you could do
```
makeSimplePath :: S.AttributeValue
makeSimplePath =  mkPath do
  l 2 3
  m "4" "5"
```
but not
```
makeSimplePath :: S.AttributeValue
makeSimplePath =  mkPath do
  l 2 3
  m 4 "5"
```

Ok damn. If there is no way to improve polymorphic type signatures with type aliases, I guess the whole idea is mood as it would be only a solution for a small subset of signatures ...

So I guess my actual question is, how can we prevent people from writing such functions: `c :: Show a => a -> a -> a -> a -> a -> a -> Path` or how can we improve the documentation for it.

I understand that people
- don't add documentation for that signature, because they are too lazy
- don't want to add newtypes for every parameter
- don't write that function differently, because it's a 1-1 translation of the SVG syntax
- don't want to jump to the source code to check out how each parameter is defined

So this are all solutions which would be nice, but are also wishful thinking.

Some ideas could be:
- Showing the `c c1x c1y c2x c2y x y =` part in Haddock (but only helps if it's not written in pointfree style)
- Think of a useful type alias style to improve the signature

Feedback / Any other ideas?
