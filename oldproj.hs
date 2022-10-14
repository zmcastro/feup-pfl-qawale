import Data.Char
--putStrLn prints a line to the console

--type setup
data Poly = Poly { symbol :: Char, coeff :: Int, var :: Char, pow :: Int } deriving (Show)

{-helper functions
polyParser :: String -> [String]
polyParser (x:xs) | (x:xs) == "" || xs == ""  = []
                  | ((x == '+') || (x == '-')) = (x : (takeWhile (\x -> ((x /= '+') && (x /= '-'))) (filterSpaces xs))) : analyzePoly (dropWhile (\x -> ((x /= '+') && (x /= '-'))) (filterSpaces xs))
                  | otherwise = (takeWhile (\x -> ((x /= '+') && (x /= '-'))) (filterSpaces (x:xs))) : analyzePoly (dropWhile (\x -> ((x /= '+') && (x /= '-'))) (filterSpaces (x:xs)))

filterSpaces :: String -> String
filterSpaces l = [x | x <- l, x /= ' ']

-- cases: a, x, ax, ax^n, [+, -]ax^n 
-- this function is unfinished!
toPoly :: String -> myPolynomial
toPoly s | (length s == 1) && (isDigit s) = Poly '+' s 'x' 0
         | (length s == 1) && (isLetter s) = Poly '+' 1 s 1 
         | (length s == 2) && (isMathematicalOperators (head s) && (isDigit (s !! 1))) = Poly (head s) (s!!1) 'x' 0
         | (length s == 2) && (isMathematicalOperators (head s) && (isLetter (s !! 1))) = Poly (head s) ()

toPoly :: String -> [a]
toPoly (x:xs) | (not (isMathematicalOperators x)) = x : xs 
-}

myminPoly :: [Poly] -> Poly 
myminPoly (x:xs) | null xs = x
                 | ((var x) == (var (head xs))) && ((pow x) < (pow (head xs))) = myminPoly (x : tail xs)
                 | ((var x) == (var (head xs))) && ((pow x) == (pow (head xs))) && ((coeff x) < (coeff (head xs))) = myminPoly (x : tail xs)
                 | otherwise = myminPoly xs  

normalizePoly :: [Poly] -> [Poly] 
normalizePoly pl | null pl = []
                 

= | any (head x) (xs) = head x : head x 