module Utils where
    import Data.List as List
    import Data.Text as Text
    import Data.Char


    {- General utility functions -}

    -- Returns true if the character is not a minus nor a digit/letter. This is used in the showPolynomialTrimmed function from the Polynomial module.
    toRemoveLeading :: Char -> Bool
    toRemoveLeading a = (a /= '-') && not (isAlphaNum a)

    -- Sums numbers of the same index in two different lists, while keeping the numbers with no "pair" in the returning list (unlike zipWith (+)).
    myListZipSum :: [Int] -> [Int] -> [Int]
    myListZipSum a [] = a
    myListZipSum [] b = b
    myListZipSum (p1:p1s) (p2:p2s) = (p1 + p2) : myListZipSum p1s p2s

    -- Lowers the degree (exponent) of a given monomial when it is represented as a string.
    lowerDegree :: String -> Int
    lowerDegree n = (read n::Int) - 1

    -- Splits a string into a list of strings, with "*" acting as the delimitor. Usually used in polynomials (hence the name of the function) but it is general purpose.
    polySplitOn :: String -> [String]
    polySplitOn s = List.sort (List.map Text.unpack (Text.splitOn (Text.pack "*") (Text.pack s)))
