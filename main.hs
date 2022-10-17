import Data.List
import Data.Sequence
import Polynomial
import Data.Binary.Get (skip)

--refactor p adicionar expoentes ou usar lista com tamanho igual ao maior expoente e guardar por ordem

createPolynomial :: Polynomial
createPolynomial = [("x", [1,6,5,0]), ("x", [2,0,-7,0]), ("", [8])]

showPolynomial :: Polynomial -> String
showPolynomial (p:ps)
                | Data.List.null (p:ps) = ""
                | Data.List.null (snd p) = showPolynomial ps
                | head (snd p) == 0 && Data.List.null ps = ""
                | head (snd p) == 0 && not (Data.List.null ps) = showPolynomial ((fst p, tail (snd p)):ps)
                | Data.List.null ps && Data.List.length (snd p) == 1 = printPoly (fst p) (head (snd p)) (Data.List.length (snd p))
                | otherwise = printPoly (fst p) (head (snd p)) (Data.List.length (snd p)) ++ " + " ++ showPolynomial ((fst p, tail (snd p)):ps)

sortAndNormalize :: Polynomial -> Polynomial
sortAndNormalize (p:ps) = normalizePolynomial (Data.List.sortOn fst (p:ps))

normalizePolynomial :: Polynomial -> Polynomial
normalizePolynomial (p:ps)
                            | Data.List.length ps == 1 && fst p /= fst (head ps) = (p:ps)
                            | Data.List.length ps == 1 && fst p == fst (head ps) = [(fst p, Data.List.zipWith (+) (snd p) (snd (head ps)))]
                            | fst p == fst (head ps) = (fst p, Data.List.zipWith (+) (snd p) (snd (head ps))) : normalizePolynomial (tail ps)
                            | otherwise = normalizePolynomial (tail ps)

printPoly :: String -> Int -> Int -> String
printPoly var coef exp
                    | exp == 0 = show coef
                    | coef == 1 && exp == 1 = var
                    | coef == 1 && exp /= 1 = var ++ "^" ++ show exp
                    | coef /= 1 && exp == 1 = show coef ++ var
                    | coef /= 1 && exp /= 1 = show coef ++ var ++ "^" ++ show exp
                    | otherwise = error "Error on printPoly"

addPolynomial :: Polynomial -> Polynomial -> Polynomial
addPolynomial [] [] = []
addPolynomial [] p = p
addPolynomial p [] = p
addPolynomial (p1:p1s) (p2:p2s) | Data.List.null p1 = (p2:p2s)
                                | Data.List.null p2 = (p1:p1s)
                                | Data.List.null p1s && Data.List.null p2s = [p1, p2]
                                | Data.List.null p1s = (addPolynomial [p1] [p2]) ++ p2s 
                                | Data.List.null p2s = (addPolynomial [p1] [p2]) ++ p1s
                                | fst p1 == fst p2 = (fst p1, [x+y |  x <- snd p1, y <- snd p2]) : addPolynomial p1s p2s
                                | otherwise = p1 : p2 : addPolynomial p1s p2s

addTest :: Polynomial
addTest = addPolynomial [("x", [0,8,6])] [("y", [1,5,4]), ("a", [1,4])]