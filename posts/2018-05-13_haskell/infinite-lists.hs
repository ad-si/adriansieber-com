import Data.Function ((&))

allNumbers = [1..]
allNumbersDoubled = allNumbers & map (*2)

main =
  allNumbersDoubled & take 5 & print
