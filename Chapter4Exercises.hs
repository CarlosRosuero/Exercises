import Data.Char
import Test.QuickCheck

angleVectors :: (Float, Float) -> (Float, Float) -> Float
angleVectors (x,y) (x',y') = acos (x*x'+y*y')/(sqrt(x^2+y^2)*sqrt(x'^2+y'^2))

type Line = (Float, Float)
intersection :: Line -> Line -> (Float, Float)
intersection (a,b) (a',b') = (x,y)
    where x=(b'-b)/(a-a')
          y=a*x+b

halveEvens :: [Int] -> [Int]
halveEvens xs = [ div x 2 | x <- xs , even x ]

inRange :: Int -> Int -> [Int] -> [Int]
inRange a b xs = [ x | x <- xs , x >= a , x <= b ]

countPositives :: [Int] -> Int
countPositives xs = length [ x | x <- xs , x >= 0 ]

multDigits :: String -> Int
multDigits xs = product [ digitToInt x | x <- xs , isDigit x ]

capitalise :: String -> String
capitalise xs = toUpper(head(xs)) : [ toLower x | x <- (tail xs) ]

prop :: [Bool] -> Bool
prop [a,b,c,d] = (( a && not b && ( c || ( d && b )) || ( not b && not a )) && c)==True

crosswordFind :: Char -> Int -> Int -> [String] -> [String]
crosswordFind letter pos len words = [ word | word <- words , length(word) == len , word!!pos==letter]

pythagoreanTriples :: [( Int , Int , Int )]
pythagoreanTriples = [ ( a , b , c ) | a <- [1..100] , b <- [1..100] , c <- [1..100] , b > a , a^2 + b^2 == c^2]
-- Adding the requirement that b be greater than a saves us from repeating triples.