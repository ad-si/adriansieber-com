Haskell Musings

\begin{code}

import Control.Arrow ((>>>))
import Control.Monad (liftM2)
import Data.Function ((&))
import Data.List (isPrefixOf)

terms =
  "street" :
  "strong" :
  "stick" :
  "what" :
  "when" :
  "wolf" :
  "something" :
  "else":
  []

goal =
  "street" :
  "strong" :
  "what" :
  "when" :
  []


withPrefix term = "str" `isPrefixOf` term || "wh" `isPrefixOf` term

withLift = liftM2 (||) ("str" `isPrefixOf`) ("wh" `isPrefixOf`)

withTake term = take 3 term == "str" || take 2 term == "wh"

withPattern ('s':'t':'r':_) = True
withPattern ('w':'h':_) = True
withPattern _ = False

withAny term = any (`isPrefixOf` term) ["str", "wh"]

withFlip = (`any` ["str", "wh"]) . (flip isPrefixOf)

withArrow = (flip isPrefixOf) >>> (`any` ["str", "wh"])

withPoint term = any (`isPrefixOf` term) ["str", "wh"]


main =
  filter withPrefix terms :
  filter withLift terms :
  filter withTake terms :
  filter withPattern terms :
  filter withAny terms :
  filter withFlip terms :
  filter withArrow terms :
  filter withPoint terms :
  []
  & map show
  & unlines
  & putStr

\end{code}
