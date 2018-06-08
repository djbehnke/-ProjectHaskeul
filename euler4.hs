{- |
  Daniel Jacob Behnke
  06/06/18
  Find the largest palindrome made from the product of two 3-digit numbers.
-}

-- | Tests whether a list is a palindrome or not
isPalindrome [] = True
isPalindrome (x:xs)
  | length(x:xs) == 1 = True
  | x == head(reverse(x:xs)) = isPalindrome (init xs)
  | otherwise = False
  
-- | converts an integer to a list, preserving order
intToList n = inner n []
  where
  inner m ls
    | m < 10 = reverse(ls ++ [m])
    |otherwise = inner (div m 10) ( ls ++ [(mod m 10)])

-- | counts down from 999 and finds if the number multiplied by n is a palindrome
digitByDigit n = inner 999
  where
  inner m
    |m< 100 = 0
    |isPalindrome(intToList(n*m)) = (n*m)
    |otherwise = inner (m-1)
    
-- | calls digitByDigit recursively from n until 99 trying to find the larger value returned
countDown n = inner n 0
  where
  inner i max
    | i < 100 = max
    | digitByDigit i > max = inner (i-1) (digitByDigit i)
    | otherwise = inner (i-1) max
