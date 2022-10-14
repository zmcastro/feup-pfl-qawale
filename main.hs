import Data.List
import Data.Sequence

import Polynomial

--refactor p adicionar expoentes ou usar lista com tamanho igual ao maior expoente e guardar por ordem

createPolynomial :: Polynomial
createPolynomial = [("y", [3,0])]

showPolynomial :: Polynomial -> String
showPolynomial (p:ps)
                | Data.List.null (p:ps) = ""
                | Data.List.null (snd p) = showPolynomial ps
                | head (snd p) == 0 = showPolynomial ((fst p, tail (snd p)):ps)
                | Data.List.length (p:ps) == 1 && Data.List.length (snd p) == 1 = printPoly (fst p) (head (snd p)) (Data.List.length (snd p) - 1)
                | otherwise = printPoly (fst p) (head (snd p)) (Data.List.length (snd p) - 1) ++ " + " ++ showPolynomial ((fst p, tail (snd p)):ps)

printPoly :: String -> Int -> Int -> String
printPoly var coef exp
                    | exp == 0 = show coef
                    | coef == 1 && exp == 1 = var
                    | coef == 1 && exp > 1 = var ++ "^" ++ show exp
                    | coef > 1 && exp == 1 = show coef ++ var
                    | coef > 1 && exp > 1 = show coef ++ var ++ "^" ++ show exp
                    | otherwise = error "Error on printPoly"

addPolynomial :: Polynomial -> Polynomial -> Polynomial
addPolynomial (p1:p1s) (p2:p2s) | Data.List.null (p1:p1s) = (p2:p2s)
                                | Data.List.null (p2:p2s) = (p1:p1s)
                               -- | fst p1 == fst p2 = (fst p1, ((\x y -> x + y) (snd p1) (snd p2))) : addPolynomial p1s p2s
                                | otherwise = p1 : p2 : addPolynomial p1s p2s