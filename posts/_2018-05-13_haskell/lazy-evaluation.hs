expensiveComputation = show (1 + 2 + 3 :: Int)
anotherExpensiveComputation = show (1 + 2 + 3 + 4 :: Int)

valueToPrint = "A"

valueA = expensiveComputation
valueB = anotherExpensiveComputation

main =
  putStrLn (if valueToPrint == "A"
    then valueA
    else valueB)
