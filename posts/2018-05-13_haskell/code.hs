{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (reverse, putStrLn)
import Data.Function ((&))
import Data.Text hiding (map, take)
import Data.Text.IO
import Data.String (fromString)
import Data.Monoid ((<>))



data ShirtSize = Small | Medium | Large

johnsSize = Medium

-- main =
--   putStrLn (case johnsSize of
--       Small -> "Eat more spinach!"
--       Medium -> "You're just average."
--       Large -> "Is the air thinner up there?"
--     )


-- allNumbers = [1..]


-- allNumbersDoubled = allNumbers & fmap (*2)

-- -- main =
-- --   allNumbersDoubled & take 5 & print

reverseAndShout :: Text -> Text
reverseAndShout text =
  let
    reversed = reverse text
    shouted = toUpper reversed
  in
    shouted

reverseAndShout' = toUpper . reverse

-- data Person = Person {firstName :: Text, lastName :: Text}



-- john :: Person
-- john = Person "John" "Doe"

-- john2 = john {lastName = "Smith"}


-- main :: IO ()
-- main =
--   putStrLn $ (reverseAndShoutName john2)

-- main :: IO ()
-- main = putStrLn $ unpack $ reverseAndShout "holy moly"
