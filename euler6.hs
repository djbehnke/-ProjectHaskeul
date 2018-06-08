{- |
  Daniel Jacob Behnke
  06/08/18
  Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.
-}

-- | Finds the difference between the sum of the squares of the first n natural numbers
sumSquareDiffernce n = (sum([1..n])^2)-(sum([ x^2 | x <- [1..n] ]))