module Main where
      import Polynomial
      import Utils
      import Tests
      import Data.List as List
      import Data.Char
      import System.Exit

      {- Main loop -}

      -- The program's main loop. Accepts user input and calls the appropriate functions. Simplified and optional way of running the main functions of the program.
      -- The polynomials used are in the Tests module and can be modified at will (as long as they respect the representation rules)

      main :: IO ()
      main = do 
            putStrLn "Welcome! Choose an option:"
            putStrLn "1 - Normalize a polynomial"
            putStrLn "2 - Add two polynomials"
            putStrLn "3 - Multiply two polynomials"
            putStrLn "4 - Differentiate a polynomial"
            putStrLn "0 - Exit"
            x <- getLine
            case x of
                  "1" -> do 
                        putStrLn (showPolynomialTrimmed (sortAndNormalize testPolynomial) ++ "\n")
                        main
                  "2" -> do 
                        putStrLn ("First polynomial: " ++ showPolynomialTrimmed (sortAndNormalize testPolynomial))
                        putStrLn ("Second polynomial: " ++ showPolynomialTrimmed (sortAndNormalize testPolynomial2))
                        putStrLn ("Result: " ++ showAddPolynomial testPolynomial testPolynomial2 ++ "\n")
                        main
                  "3" -> do 
                        putStrLn ("First polynomial: " ++ showPolynomialTrimmed (sortAndNormalize testPolynomial))
                        putStrLn ("Second polynomial: " ++ showPolynomialTrimmed (sortAndNormalize testPolynomial2))
                        putStrLn ("Result: " ++ showMultiplyPolynomial testPolynomial testPolynomial2 ++ "\n")
                        main
                  "4" -> do 
                        putStrLn ("Polynomial to be differentiated: " ++ showPolynomialTrimmed (sortAndNormalize testPolynomial))
                        putStrLn "Which variable do you want to differentiate?"
                        var <- getLine
                        if List.null var || List.length var > 1 || not (isAlpha (head var)) then 
                              do 
                              putStrLn "Only single letter variables are supported (x, y, w...).\n"
                              main
                        else do 
                              putStrLn ("Chosen variable: " ++ var)
                              putStrLn ("Result: " ++ showDerivePartial var testPolynomial ++ "\n")
                              main
                  "0" -> do 
                        putStrLn "Goodbye!"
                        exitSuccess
                  _ -> do 
                        putStrLn "Invalid option!"
                        main