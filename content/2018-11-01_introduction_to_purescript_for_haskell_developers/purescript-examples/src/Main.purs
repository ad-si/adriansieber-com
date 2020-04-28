module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)


type Book = { title :: String, author :: String, year :: Int }

type Book2 = ( title :: String, author :: String, year :: Int )


book :: {title :: String, author :: String, year :: Int}
book =
  { title: "Eine Woche voller Samstage"
  , author: "Paul Maar"
  , year: 1973
  }


constantLength :: forall a. Array a -> Int
constantLength _ = 5


type Point = {x :: Int, y :: Int}


showPoint :: Point -> String
showPoint p =
  show p.x <> ", " <> show p.y


sentence :: String
sentence = """This is
just some text
split over several lines
"""


showPrint :: forall a. {title :: String, author :: String | a} -> String
showPrint b = b.title <> " by " <> b.author


showBook :: Book -> String
showBook b = b.title <> " by " <> b.author


add3 :: Int -> Int -> Int -> Int
add3 valA valB valC =
  valA + valB + valC


main :: Effect Unit
main = do
  log $ _.title book
  log $ _.title {title: "Just a title"}
  log $ showPoint {x: 1, y: 2}
  log $ "======" <> sentence <> "======"
  log $ showPrint book
