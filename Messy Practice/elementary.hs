sqr::Float->Float
sqr(n) = n*n

cube::Float->Float
cube n = sqr(n) * n

fourth::Float->Float
fourth = sqr.sqr

power::Float -> Integer -> Float
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

rhs::Integer->Integer
rhs n = div (n*(n+1)) 2

check::Integer->Integer
check n = sumUpTo(n) - rhs(n)