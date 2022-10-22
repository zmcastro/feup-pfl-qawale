module Utils where
    import Data.List
    import Data.Char
    import Control.Exception

    catchException :: IO a -> (SomeException -> IO a) -> IO a
    catchException = Control.Exception.catch

    toRemoveLeading :: Char -> Bool
    toRemoveLeading a = (a /= '-') && not (isAlphaNum a)

    parseFirstExp :: String -> String
    parseFirstExp a = takeWhile isDigit (dropWhile (not . isDigit) a)

    myListSum :: [Int] -> [Int] -> [Int]
    myListSum a [] = a
    myListSum [] b = b
    myListSum (p1:p1s) (p2:p2s) = (p1 + p2) : myListSum p1s p2s
