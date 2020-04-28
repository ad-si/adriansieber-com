#! /usr/bin/env stack
{- stack
  script
  --resolver lts-10.4
-}

import Data.Bool (bool)
import Control.Monad (ap)
import Control.Applicative ((<*>))
import Data.Function ((&))

-- Generated with pointfree.io from
-- `applyIf testFunc doThis value = bool value (doThis value) (testFunc value)`
applyIf :: (a -> Bool) -> (a -> a) -> a -> a
applyIf = flip ((<*>) . (<*>) bool)
--                   v--v


-- Generated with pointfree.io from
-- `applyWhen p f a = maybe a f $ p a`
applyWhen :: (a -> Maybe a) -> (a -> a) -> a -> a
applyWhen = flip (ap . flip maybe)


-- mApplyIf :: Monad m => (m a -> Bool) -> (m a -> m a) -> m a -> m a
-- mApplyIf = flip (ap . ap bool)
-- -- applyIf testFunc doThis value = bool value (doThis value) (testFunc value)


(??) = applyIf
infixl 9 ??


things = [1, 2, 3, 4]


main = do
  things
    & fmap ((> 2) `applyIf` (+5))
    & show
    & putStrLn

  things
    & fmap ((> 2) ?? (+5))
    & show
    & putStrLn

  -- things
  --   & fmap ((> 2) `mApplyIf` (+5))
  --   & show
  --   & putStrLn
