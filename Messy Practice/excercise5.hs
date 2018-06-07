myMax::Integer->Integer->Integer->Integer
myMax i j k
  | i >= j = inner i k
  | j >= i = inner j k
    where
    inner p q
      | p >= q = p
      | q >= p = q

myMin::Integer->Integer->Integer->Integer
myMin i j k
  | i <= j = inner i k
  | j <= i = inner j k
    where
    inner p q
      | p <= q = p
      | q <= p = q
      
myMean::Integer->Integer->Integer->Integer
myMean i j k = div (i+j+k) 3

xor::Bool->Bool->Bool
xor a b = a && (not b) || b && (not a)

myMed::Integer->Integer->Integer->Integer
myMed i j k
  | xor (i <= j) (i <= k) = i
  | xor (j <= i) (j <= k) = j
  | xor (k <= i) (k <= j) = k
  | otherwise = i
  
fibbonacci::Integer->Integer
fibbonacci 1 = 1
fibbonacci 2 = 1
fibbonacci n = inner (n-1) 1 1
  where
  inner a b c
    |a == 0 = (b+c)
    |otherwise = inner (a-1) (c) (b+c)
    
myGCD::Integer->Integer->Integer
myGCD n m = inner (abs n) (abs m)
  where 
  inner n m
    | n < m = inner m n 
    | mod n m == 0 = m
    | otherwise = inner m (mod n m)
    
areRelativePrimes::Integer->Integer->Bool
areRelativePrimes n m
  |myGCD n m == 1 = True
  |otherwise = False
  
  
largestDivisor::Integer->Integer
largestDivisor n = inner n ((quot n 2)+1)
  where
  inner a b
    | mod a b == 0 = b
    | otherwise = inner a (b-1)
    
isPrime::Integer->Bool
isPrime n
  | largestDivisor n == 1 = True
  | otherwise = False
  
bigPrime::Integer->Integer
bigPrime n
  | isPrime n = n
  | otherwise = bigPrime (n+2)
  
intLog::Integer->Integer->Integer
intLog n m
  | n > m = 0
  | n == m = 1
  | otherwise = inner n m 2
    where
    inner a b i
      | a^i > b = (i-1)
      |otherwise = inner a b (i+1)
      
sumOfDigits::Integer->Integer
sumOfDigits n = inner n 0
  where
  inner n t
    |n < 10 = (n + t)
    |otherwise = inner (div n 10) ((mod n 10) + t)

sumOfSquareDigits::Integer->Integer
sumOfSquareDigits n = inner n 0
  where
  inner n t
    |n < 10 = (n^2 + t)
    |otherwise = inner (div n 10) (((mod n 10)^2) + t)

intReverse::Integer->Integer
intReverse n = inner n 0 ((intLog 10 n)) 
  where
  inner n m l
    |l == 0 = m + n
    |otherwise = inner (div n 10) (10^l*(mod n 10)+m) (l-1)
    
divisors::Integer->[Integer]
divisors n
  |n == 1 = [1]
  | otherwise = inner n ((div n 2)+1) [n]
  where
  inner a b z
    | b == 1 = 1:z
    | mod a b == 0 = inner a (b-1) (b:z)
    |otherwise = inner a (b-1) z
    
isPerfect::Integer->Bool
isPerfect n
  | (sum (divisors n) - n )== n = True
  | otherwise = False