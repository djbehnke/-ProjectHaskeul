{- |
  Daniel Jacob Behnke
  06/06/18
  Finds the largest prime factor of a given number
-}

-- | A simple implementation for finding if a number is prime
lazySieve::Integer->Bool
lazySieve n = inner n 2
  where
  inner n i
    | i == (iSqrt(n) + 1) = True
    | mod n i == 0 = False
    | otherwise = inner n (i+1)
  
-- | Finds the largest prime divisor of a number n
largestPrimeDivisor::Integer->Integer
largestPrimeDivisor n = inner n (iSqrt(n))
  where
  inner a b
    | mod a b == 0 && lazySieve(b) = b
    | otherwise = inner a (b-1)
    
-- | Calculates an integer square root where iSqrt(n) = x, x^2 <= n
iSqrt::Integer->Integer
iSqrt n = inner n 0
  where
  inner n i
    | i^2 > n = (i-1)
    | otherwise = inner n (i+1)