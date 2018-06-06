{- |
  Daniel Jacob Behnke
  06/06/18
  Sum all of the even fibonacci numbers below four million
-}

-- | This function will sum all the even fibonacci elements that are even up to an integer n
fibonacci::Integer->Integer
fibonacci n = inner 1 1 0
  where
  inner a b t
    | b >= n = t
    | mod (a+b) 2 == 0 =  inner (b) (a+b) (t+a+b)
    | otherwise = inner  (b) (a+b) t
    
