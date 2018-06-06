{- |
  Daniel Jacob Behnke
  06/06/2018
  Haskell-style solution to the Euler Project Problem 1
 -}
 
 -- | Sums the first n multiples of three recursively
sumThrees::Int->Int
sumThrees n = inner 0 0
  where
  inner m t
    |m == n = t
    |otherwise = inner (m+1) (t+3*(m+1))
    
-- | Sums the first n multiples of five recursively
sumFives::Int->Int
sumFives n = inner 0 0
  where
  inner m t 
    |m ==n = t
    |otherwise = inner (m+1) (t+5*(m+1))

-- | Sums the first n multiples of fifteen recursively
sumFifteens::Int->Int
sumFifteens n = inner 0 0
  where
  inner m t 
    |m ==n = t
    |otherwise = inner (m+1) (t+15*(m+1))

-- | sums all of the multiples of three or five that are below n
multiplesOfThreeAndFive::Int->Int
multiplesOfThreeAndFive n = sumThrees(div n 3) + sumFives(div n 5) - sumFifteens(div n 15)