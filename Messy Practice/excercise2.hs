sqr::Integer->Integer
sqr(n) = n*n

cube::Integer->Integer
cube n = sqr(n) * n

fourth::Integer->Integer
fourth = sqr.sqr

power::Integer -> Integer -> Integer
power x 0 = 1
power x m = x * power x (m-1) 

factorial::Integer->Integer
factorial 0 = 1 
factorial n 
  | n > 0 = n * factorial(n-1)
  | n < 0 = factorial(-n)
  
sumUpTo::Integer->Integer
sumUpTo 1 = 1
sumUpTo n = n + sumUpTo(n-1)


sumSquaresUpTo::Integer->Integer
sumSquaresUpTo 1 = 1
sumSquaresUpTo n = sqr(n) + sumSquaresUpTo(n-1)

sumCubesUpTo:: Integer->Integer
sumCubesUpTo 1 = 1
sumCubesUpTo n = cube(n) + sumCubesUpTo(n-1)

sumPowersUpTo::Integer->Integer -> Integer
sumPowersUpTo n 1 = n
sumPowersUpTo n m = power n m + power n (m-1)

checkSumSquares::Integer->Integer
checkSumSquares n = sumSquaresUpTo n - div (n*(n+1)*(2*n+1)) 6

checkSumCubes:: Integer->Integer
checkSumCubes n = sumCubesUpTo n - sqr(div (n*(n+1)) 2)

iSqrt::Integer->Integer
iSqrt n = inner n 0
  where
  inner n i
    | sqr(i) > n = (i-1)
    | otherwise = inner n (i+1)


aSeq::Integer->Integer
aSeq 1 = 2
aSeq n = 2*aSeq(n-1) + 1

bSeq::Integer->Integer
bSeq n = 2^n + 2^(n-1) -1