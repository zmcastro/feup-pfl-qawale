import Data.List
import Data.Sequence
import Data.Char
import Polynomial
import Data.Binary.Get (skip)

toRemoveLeading :: Char -> Bool
toRemoveLeading a = (a /= '-') && not (isAlphaNum a)

--refactor p adicionar expoentes ou usar lista com tamanho igual ao maior expoente e guardar por ordem

createPolynomial :: Polynomial
createPolynomial = [("", [-1,6,-0,-4]), ("w", [-1,-6,-0,-4]), ("y", [-1,-6,-0,-4]), ("x", [-1,-6,-0,-4]), ("z", [-1,-6,-0,-4])]

showPolynomialTrimmed :: Polynomial -> String
showPolynomialTrimmed (p:ps)
                            | head a == '-' = '-' : Data.List.drop 2 a
                            | otherwise = a
                            where a = dropWhile (toRemoveLeading) (showPolynomial (p:ps))

showPolynomial :: Polynomial -> String
showPolynomial (p:ps)
                | Data.List.null (p:ps) = ""
                | Data.List.null (snd p) = showPolynomial ps
                | Data.List.null ps && Data.List.length (snd p) == 1 && head (snd p) < 0 = " - " ++ printPolynomial (fst p) (abs (head (snd p))) (Data.List.length (snd p))
                | Data.List.null ps && Data.List.length (snd p) == 1 && head (snd p) > 0 = " + " ++ printPolynomial (fst p) (head (snd p)) (Data.List.length (snd p))
                | head (snd p) == 0 = printPolynomial (fst p) (head (snd p)) (Data.List.length (snd p)) ++ showPolynomial ((fst p, tail (snd p)):ps)
                | head (snd p) < 0 = " - " ++ printPolynomial (fst p) (abs (head (snd p))) (Data.List.length (snd p)) ++ showPolynomial ((fst p, tail (snd p)):ps)
                | otherwise = " + " ++ printPolynomial (fst p) (head (snd p)) (Data.List.length (snd p)) ++ showPolynomial ((fst p, tail (snd p)):ps)

sortAndNormalize :: Polynomial -> Polynomial
sortAndNormalize (p:ps)
                        | (fst (head a)) == "" = tail (a) ++ [head a]
                        | otherwise = a
                        where a = normalizePolynomial(Data.List.sortOn fst (p:ps))

normalizePolynomial :: Polynomial -> Polynomial
normalizePolynomial (p:ps)
                            | Data.List.null ps && not (Data.List.null (fst p)) = [p]
                            | Data.List.length ps == 1 && fst p /= fst (head ps) = p:ps
                            | Data.List.length ps == 1 && fst p == fst (head ps) = [(fst p, Data.List.zipWith (+) (snd p) (snd (head ps)))]
                            | Data.List.null (fst p) && Data.List.length (snd p) /= 1 && Data.List.null ps = [(fst p, [Data.List.sum (snd p)])]
                            | Data.List.null (fst p) && Data.List.length (snd p) /= 1 = normalizePolynomial ((fst p, [Data.List.sum (snd p)]) : ps)
                            | fst p == fst (head ps) = normalizePolynomial ((fst p, Data.List.zipWith (+) (snd p) (snd (head ps))) : tail ps)
                            | otherwise = p : normalizePolynomial ps

printPolynomial :: String -> Int -> Int -> String
printPolynomial var coef exp
                    | coef == 0 = ""
                    | coef == 1 && Data.List.null var = show coef
                    | coef == 1 && exp == 1 = var
                    | coef == 1 && exp /= 1 = var ++ "^" ++ show exp
                    | coef /= 1 && exp == 1 = show coef ++ var
                    | coef /= 1 && exp /= 1 = show coef ++ var ++ "^" ++ show exp
                    | otherwise = error "Error on printPolynomial"

addPolynomial :: Polynomial -> Polynomial -> Polynomial
addPolynomial [] [] = []
addPolynomial [] p = p
addPolynomial p [] = p
addPolynomial p1 p2 = sortAndNormalize (p1 ++ p2)

addTest :: Polynomial
addTest = addPolynomial [("x", [0,8,6])] [("y", [1,5,4]), ("a", [1,4]), ("x", [1,5,9])]

-- Derivation Functions
derivatePolynomial :: Polynomial -> Polynomial
derivatePolynomial [] = []
derivatePolynomial (x:xs) | (fst x) == "" = x : derivatePolynomial xs 
                          | otherwise = [(fst x, derivatePolyCoeffs (snd x) 0)] ++ derivatePolynomial xs 

testDerivate :: Polynomial
testDerivate = derivatePolynomial [("", [-1,6,-0,-4]), ("w", [-1,-6,-0,-4]), ("y", [-1,-6,-0,-4]), ("x", [-1,-6,-0,-4]), ("z", [-1,-6,-0,-4])]

derivatePolyCoeffs :: [Coefficient] -> Int -> [Coefficient]
derivatePolyCoeffs [] n = []
derivatePolyCoeffs (x:xs) 0 = derivatePolyCoeffs xs 1 
derivatePolyCoeffs (x:xs) n = [x*n] ++ derivatePolyCoeffs xs (n+1)

testDerivateCoeffs :: [Coefficient]
testDerivateCoeffs = derivatePolyCoeffs([1,4,8]) 0 
--
