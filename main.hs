import Data.List
import qualified Data.Text as Text
import Data.Sequence
import Data.Char
import Polynomial
import Data.Foldable (Foldable(toList))

-- multiplicação de variáveis compostas: splitOn (*)

toRemoveLeading :: Char -> Bool
toRemoveLeading a = (a /= '-') && not (isAlphaNum a)

parseFirstExp :: String -> String
parseFirstExp a = takeWhile isDigit (dropWhile (not . isDigit) a)

sortCompositeVariables :: (Variable, [Coefficient]) -> (Variable, [Coefficient]) -> Ordering
sortCompositeVariables p1 p2
                        | Data.List.null (fst p1) = GT
                        | Data.List.null (fst p2) = LT
                        | head (fst p1) < head (fst p2) = LT
                        | head (fst p1) > head (fst p2) = GT
                        | var_eq && single_first_var = LT
                        | var_eq && single_second_var = GT
                        | var_eq && single_vars = EQ
                        | var_eq && fst p1 !! 1 /= '^' && fst p2 !! 1 == '^' = GT
                        | var_eq && fst p2 !! 1 /= '^' && fst p1 !! 1 == '^' = LT
                        | parseFirstExp (fst p1) < parseFirstExp (fst p2) = GT
                        | parseFirstExp (fst p1) > parseFirstExp (fst p2) = LT
                        | Data.List.length (fst p1) < Data.List.length (fst p2) = LT
                        | Data.List.length (fst p1) > Data.List.length (fst p2) = GT
                        | otherwise = EQ
                        where single_vars = Data.List.length (fst p1) == 1 && Data.List.length (fst p2) == 1
                              single_first_var = Data.List.length (fst p1) == 1 && Data.List.length (fst p2) > 1
                              single_second_var = Data.List.length (fst p1) > 1 && Data.List.length (fst p2) == 1
                              var_eq = head (fst p1) == head (fst p2)

createPolynomial :: Polynomial
createPolynomial = [("x^2*y^3", [6])]

createPolynomial2 :: Polynomial
createPolynomial2 = [("x^2*y^2", [3])]

showPolynomialTrimmed :: Polynomial -> String
showPolynomialTrimmed (p:ps)
                            | head a == '-' = '-' : Data.List.drop 2 a
                            | otherwise = a
                            where a = dropWhile toRemoveLeading (showPolynomial (p:ps))

showPolynomial :: Polynomial -> String
showPolynomial (p:ps)
                | Data.List.null (p:ps) = ""
                | Data.List.null (snd p) = showPolynomial ps
                | last_monomial && head (snd p) < 0 = " - " ++ printPolynomial (fst p) (abs (head (snd p))) (Data.List.length (snd p))
                | last_monomial && head (snd p) == 0 = "" ++ printPolynomial (fst p) (head (snd p)) (Data.List.length (snd p))
                | last_monomial && head (snd p) > 0 = " + " ++ printPolynomial (fst p) (head (snd p)) (Data.List.length (snd p))
                | head (snd p) == 0 = printPolynomial (fst p) (head (snd p)) (Data.List.length (snd p)) ++ showPolynomial ((fst p, tail (snd p)):ps)
                | head (snd p) < 0 = " - " ++ printPolynomial (fst p) (abs (head (snd p))) (Data.List.length (snd p)) ++ showPolynomial ((fst p, tail (snd p)):ps)
                | otherwise = " + " ++ printPolynomial (fst p) (head (snd p)) (Data.List.length (snd p)) ++ showPolynomial ((fst p, tail (snd p)):ps)
                where last_monomial = Data.List.null ps && Data.List.length (snd p) == 1

sortAndNormalize :: Polynomial -> Polynomial
sortAndNormalize (p:ps)
                        | fst (head a) == "" = tail a ++ [head a]
                        | otherwise = a
                        where a = normalizePolynomial(Data.List.sortBy sortCompositeVariables (p:ps))

normalizePolynomial :: Polynomial -> Polynomial
normalizePolynomial (p:ps)
                            | Data.List.null ps && not (Data.List.null (fst p)) = [p]
                            | Data.List.length ps == 1 && fst p /= fst (head ps) = p:ps
                            | Data.List.length ps == 1 && fst p == fst (head ps) = [(fst p, Data.List.reverse (myListSum (Data.List.reverse (snd p)) (Data.List.reverse (snd (head ps)))))]
                            | Data.List.null (fst p) && Data.List.length (snd p) /= 1 && Data.List.null ps = [(fst p, [Data.List.sum (snd p)])]
                            | Data.List.null (fst p) && Data.List.length (snd p) /= 1 = normalizePolynomial ((fst p, [Data.List.sum (snd p)]) : ps)
                            | fst p == fst (head ps) = normalizePolynomial ((fst p, Data.List.reverse (myListSum (Data.List.reverse (snd p)) (Data.List.reverse (snd (head ps))))) : tail ps)
                            | otherwise = p : normalizePolynomial ps

myListSum :: [Coefficient] -> [Coefficient] -> [Coefficient]
myListSum a [] = a
myListSum [] b = b
myListSum (p1:p1s) (p2:p2s) = (p1 + p2) : myListSum p1s p2s

printPolynomial :: String -> Int -> Int -> String
printPolynomial var coef exp
                    | coef == 0 = ""
                    | coef == 1 && Data.List.null var = show coef
                    | coef == 1 && exp == 1 = var
                    | coef == 1 && exp /= 1 = var ++ "^" ++ show exp
                    | coef /= 1 && exp == 1 = show coef ++ var
                    | coef /= 1 && exp /= 1 = show coef ++ var ++ "^" ++ show exp
                    | otherwise = error "Error on printPolynomial"

addPolynomial :: Polynomial -> Polynomial -> String
addPolynomial a b = showPolynomialTrimmed (sortAndNormalize (a ++ b))

multiplyPolynomials :: Polynomial -> Polynomial -> Polynomial
multiplyPolynomials (p1:p1s) (p2:p2s)
                                    | Data.List.null p1s = multPolyToList [p1] (p2:p2s)
                                    | otherwise = multPolyToList [p1] (p2:p2s) ++ multiplyPolynomials p1s (p2:p2s)

multPolyToList :: Polynomial -> Polynomial -> Polynomial
multPolyToList a (p:ps)
                        | Data.List.null (fst p) && Data.List.null ps = [(fst (head a), map (* head (snd p)) (snd (head a)))]
                        | Data.List.null (fst p) = (fst (head a), map (* head (snd p)) (snd (head a))) : multPolyToList a ps
                        | Data.List.null (fst (head a)) && Data.List.null ps = [(fst p, map (* head (snd (head a))) (snd p))]
                        | Data.List.null (fst (head a)) = (fst p, map (* head (snd (head a))) (snd p)) : multPolyToList a ps
                        | (Data.List.length (fst p) > 1 || Data.List.length (fst (head a)) > 1) && Data.List.null ps = multCompositeVariables a [p]
                        | Data.List.length (fst p) > 1 || Data.List.length (fst (head a)) > 1 = multCompositeVariables a [p] ++ multPolyToList a ps
                        | fst (head a) == fst p && Data.List.null ps = [(fst p, multSameVar (snd (head a)) (snd p) new_list)]
                        | fst (head a) == fst p = (fst p, multSameVar (snd (head a)) (snd p) new_list) : multPolyToList a ps
                        | fst (head a) /= fst p && Data.List.null ps = multDiffVar a [p]
                        | fst (head a) /= fst p = multDiffVar a [p] ++ multPolyToList a ps
                        where new_list = Data.List.replicate (Data.List.length (snd (head a)) + Data.List.length (snd p)) 0

multSameVar :: [Coefficient] -> [Coefficient] -> [Coefficient] -> [Coefficient]
multSameVar a b c
                    | Data.List.null (tail a) = multSingleSameVar (head a) len b c
                    | otherwise = multSameVar (tail a) b (multSingleSameVar (head a) len b c)
                    where len = Data.List.length a

multSingleSameVar :: Coefficient -> Int -> [Coefficient] -> [Coefficient] -> [Coefficient]
multSingleSameVar a l b c
                    | Data.List.null (tail b) = toList (Data.Sequence.update idx new_coefficient (fromList c))
                    | otherwise = multSingleSameVar a l (tail b) (toList (Data.Sequence.update idx new_coefficient (fromList c)))
                    where idx = Data.List.length c - l - Data.List.length b; new_coefficient = a * head b + index (fromList c) idx

multDiffVar :: Polynomial -> Polynomial -> Polynomial
multDiffVar (p1:p1s) (p2:p2s)
                | Data.List.length (snd p1) == 1 = multSingleDiffVar (fst p1) (head (snd p1)) (Data.List.length (snd p1)) [p2]
                | otherwise = multSingleDiffVar (fst p1) (head (snd p1)) (Data.List.length (snd p1)) [p2] ++ multDiffVar [(fst p1, tail (snd p1))] [p2]

multSingleDiffVar :: Variable -> Coefficient -> Exponent -> Polynomial -> Polynomial
multSingleDiffVar v c e (p:ps)
                | Data.List.length (snd p) == 1 = [(orderedPrint v (fst p) e (Data.List.length (snd p)), [c * head (snd p)])]
                | otherwise = (orderedPrint v (fst p) e (Data.List.length (snd p)), [c * head (snd p)]) : multSingleDiffVar v c e [(fst p, tail (snd p))]

multCompositeVariables :: Polynomial -> Polynomial -> Polynomial
multCompositeVariables (p1:p1s) (p2:p2s)
                                          | Data.List.length (snd p1) == 1 && Data.List.length (fst p1) == 1 && Data.List.length (fst p2) > 1 = iterate_first
                                          | Data.List.length (snd p2) == 1 && Data.List.length (fst p1) > 1 && Data.List.length (fst p2) == 1 = iterate_second
                                          | Data.List.length (snd p1) /= 1 && Data.List.length (fst p1) == 1 && Data.List.length (fst p2) > 1 = iterate_first ++ multCompositeVariables [(fst p1, tail (snd p1))] (p2:p2s)
                                          | Data.List.length (snd p2) /= 1 && Data.List.length (fst p1) > 1 && Data.List.length (fst p2) == 1 = iterate_second ++ multCompositeVariables (p1:p1s) [(fst p1, tail (snd p1))]
                                          | otherwise = [(parseCompositeVariables (polySplitOn (fst p1) ++ fst p2), [head (snd p1) * head (snd p2)])]
                                          where iterate_first = multSingleCompositeVariables (fst p1) (head (snd p1)) (Data.List.length (snd p1)) [p2]
                                                iterate_second = multSingleCompositeVariables (fst p2) (head (snd p2)) (Data.List.length (snd p2)) [p1]

multSingleCompositeVariables :: Variable -> Coefficient -> Exponent -> Polynomial -> Polynomial
multSingleCompositeVariables v c e (p:ps) = [(parseCompositeVariables (polySplitOn (v ++ "^" ++ show e ++ "*" ++ fst p)), [c * head (snd p)])]

parseCompositeVariables :: [Variable] -> Variable
parseCompositeVariables (p1:p1s)
                                    |

polySplitOn :: String -> [String]
polySplitOn s = Data.List.sort (Data.List.map Text.unpack (Text.splitOn (Text.pack "*") (Text.pack s)))

orderedPrint :: Variable -> Variable -> Exponent -> Exponent -> String
orderedPrint v1 v2 e1 e2
                        | v1 < v2 = printPolynomial v1 1 e1 ++ "*" ++ printPolynomial v2 1 e2
                        | otherwise = printPolynomial v2 1 e2 ++ "*" ++ printPolynomial v1 1 e1


-- Derivation Functions --

-- Main composite function for calculating and outputting derivatives
deriveOp :: Polynomial -> String
deriveOp = (showPolynomial . sortAndNormalize . derivePolynomial) 

-- Main function for for calculating and outputting partial derivatives
derivePartialOp :: Variable -> Polynomial -> String
derivePartialOp var p = showPolynomial(sortAndNormalize (derivePartial var p))

-- Derivation function based on every variable present in the polynomial
derivePolynomial :: Polynomial -> Polynomial
derivePolynomial [] = []
derivePolynomial (x:xs) | (fst x) == "" = [(fst x, [sum ((snd x) ++ [sumNewConstants (x:xs)])])] ++ derivePolynomial xs 
                          | otherwise = [(fst x, derivePolyCoeffs (init (snd x)) 2)] ++ derivePolynomial xs

testderive :: String
testderive = deriveOp [("", [-1,6,-0,-4]), ("w", [-1,-6,-0,-4]), ("y", [-1,-6,-0,-4]), ("x", [-1,-6,-0,-4]), ("z", [-1,-6,-0,-4])]

-- Function that returns value of new constants that appeared from derivation on degree-one monomials
sumNewConstants :: Polynomial -> Int
sumNewConstants p = sum [x | x <- [last (snd i) | i <- p, (fst i) /= ""]]
                  
testSumNew :: Int
testSumNew = sumNewConstants [("", [-1,6,-0,-4]), ("w", [-1,-6,-0,-4]), ("y", [-1,-6,-0,-4]), ("x", [-1,-6,-0,-4]), ("z", [-1,-6,-0,-4])]

-- Partial derivation functionbased on 1 user-input variable
derivePartial :: Variable -> Polynomial -> Polynomial
derivePartial _ [] = []
derivePartial var (x:xs) | (fst x) == "" = [(fst x, [sum ((snd x) ++ [sumNewConstantsPartial var x])])] ++ derivePartial var xs 
                           | (fst x) == var = [(fst x, derivePolyCoeffs (init (snd x)) 2)] ++ derivePartial var xs
                           | otherwise = x : derivePartial var xs

testderivePartial :: String
testderivePartial = derivePartialOp "x" [("", [-1,6,-0,-4]), ("w", [-1,-6,-0,-4]), ("y", [-1,-6,-0,-4]), ("x", [3]), ("z", [-1,-6,-0,-4])] 

-- Function that returns value of the new constant coming from derivation on degree-one monomial
sumNewConstantsPartial :: Variable -> (Variable, [Coefficient]) -> Int
sumNewConstantsPartial var p = if Data.List.null (snd p) then 0 else last (snd p)

-- Recursive derivation function that returns lists of coefficients
derivePolyCoeffs :: [Coefficient] -> Int -> [Coefficient]
derivePolyCoeffs [] n = []
derivePolyCoeffs p 0 = derivePolyCoeffs (init p) 1 
derivePolyCoeffs p n = derivePolyCoeffs (init p) (n+1) ++ [(last p) *n]

testderiveCoeffs :: [Coefficient]
testderiveCoeffs = derivePolyCoeffs ([1,4,8]) 0 

------ 


