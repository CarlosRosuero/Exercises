import Test.QuickCheck

max3 :: Int -> Int -> Int -> Int
max3 a b c
    | a >= b && a >= c = a
    | b >= a && b >= c = b
    | otherwise        = c 

max3_prop :: Int -> Int -> Int -> Bool
max3_prop a b c = ((max3 a b c) >= a) && ((max3 a b c) >= b) && ((max3 a b c) >= c)

rev2_prop :: [Int] -> Bool
rev2_prop xs = reverse (reverse xs ) == xs

addmult_prop :: Int -> Int -> Int -> Bool
addmult_prop a b c = ( a + b == b + a ) && ( a * b == b * a ) && ( a + ( b + c ) == ( a + b ) + c ) && ( a * ( b * c ) == ( a * b ) * c )

subs_prop :: Int -> Int -> Bool
subs_prop m n = ( m + n ) - n == m

div_prop :: Float -> Float -> Property
div_prop m n = 
    n > 0 ==> ( m * n ) / n == m
-- Fails for Floats because of precision (I think)

