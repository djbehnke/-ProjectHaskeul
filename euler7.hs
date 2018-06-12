{- |
  Daniel Jacob Behnke
  06/08/18
  What is the 10 001st prime number?
-}

-- | this generates a large list of primes and selects the mth one. Will not work for larger numbers
nthPrime m = [n | n<-[2..(2^m)], all ((> 0).rem n) [2..floor.sqrt.fromIntegral$n]]!!(m-1)
