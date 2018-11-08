{-# LANGUAGE OverloadedStrings #-}

-- import Prelude hiding (reverse, putStrLn)
-- import Data.Function ((&))
-- import Data.Text
-- import Data.Text.IO
-- import Data.Monoid ((<>))


-- reverseAndShout :: Text -> Text
-- reverseAndShout text =
--   let
--     reversed = reverse text
--     shouted = toUpper reversed
--   in
--     shouted

-- main :: IO ()
-- main =
--   putStrLn $ unpack $ reverseAndShout "holy moly"



-- data Person = Person {firstName :: Text, lastName :: Text}

-- reverseAndShoutName :: Person -> Text
-- reverseAndShoutName person =
--   let fullName = firstName person <> lastName person
--   in fullName & reverse & toUpper

-- john :: Person
-- john = Person "John" "Doe"

-- john2 = john {lastName = "Smith"}

-- main :: IO ()
-- main =
--   putStrLn $ (reverseAndShoutName john2)




