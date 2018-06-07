factorial::Integer->Integer
factorial 0 = 1 
factorial m 
  | m > 0 = m * factorial(m-1)
  | m < 0 = factorial(-m)