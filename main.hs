module Main where
      import Polynomial
      import Utils
      
      -- The program's main loop. Accepts user input and calls the appropriate functions 

      main :: IO [Polynomial]
      main = do 
            putStrLn "Welcome!"
            x <- getLine
            let opt = read x :: Int
            case opt of 
                  1 -> return createPolynomial
                  _ -> error "Nope"




