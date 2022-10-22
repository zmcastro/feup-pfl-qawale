module Utils where
    import Data.List
    import Data.Char
    import Control.Exception

    catchException :: IO a -> (SomeException -> IO a) -> IO a
    catchException = Control.Exception.catch

    toRemoveLeading :: Char -> Bool
    toRemoveLeading a = (a /= '-') && not (isAlphaNum a)

    myListZipSum :: [Int] -> [Int] -> [Int]
    myListZipSum a [] = a
    myListZipSum [] b = b
    myListZipSum (p1:p1s) (p2:p2s) = (p1 + p2) : myListZipSum p1s p2s
