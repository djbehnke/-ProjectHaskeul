{- |
  Daniel Jacob Behnke
  06/08/18
  What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
-}

-- | Finds the smallest number that is a value set in modBy20 by recursively trying larger numbers
divBy n
  | modBy20 n == True = n
  | otherwise = divBy (n+2520)
  
-- | checks if n is divisible by [1..20]
modBy20 n = inner 2
  where
  inner m
    | m > 20 = True
    | not (mod n m ==0) = False
    | otherwise = inner (m+1)