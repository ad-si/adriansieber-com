{-# LANGUAGE NoImplicitPrelude #-}

import Prelude (Char, IO)
import qualified Prelude


sayHi :: ([[Char]] -> a) -> [Char] -> a
sayHi unwords name =
  unwords ["Hi", name, ",how", "is", "it", "going?"]


main :: IO ()
main =
  Prelude.putStrLn (sayHi Prelude.unwords "John")
