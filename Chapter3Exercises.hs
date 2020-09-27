root :: Float -> Float -> Float -> Float
root a b c = ((-b)+sqrt(b*b-4*a*c))/(2*a)

hour :: Int -> Int
hour x = mod ( 1 + div x 60 ) 12

between :: Int -> Int -> Int -> Int
between a b c
    | (a>b && a<c) || (a>c && a<b) = a
    | (b>a && b<c) || (b>c && b<a) = b
    | otherwise                    = c

xor :: Bool -> Bool -> Bool
xor a b
    | a&&b      = False
    | otherwise = a||b

or1 :: Bool -> Bool -> Bool
or1 a b = if a==b && a==False then False else True --Most elegant

or2 :: Bool -> Bool -> Bool
or2 a b
    | a==b && a==False = False
    | otherwise = True --Easier to understand

or3 :: Bool -> Bool -> Bool
or3 True True = True
or3 True False = True
or3 False True = True
or3 False False = False -- Quickest (no calculations)
