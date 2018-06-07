inOrder::Int->Int->Int->Bool
inOrder i j k = i<j && j<k 

isSqr::Integer->Bool
isSqr n = inner n 0
  where
  inner n i
    | i^2 == n = True
    | i > div n 2 = False
    | otherwise = inner n (i+1)

isSqrP::Integer->Bool
isSqrP n = floor(sqrt(fromIntegral n))^2 == n

xor::Bool->Bool->Bool
xor a b = a && (not b) || b && (not a)
