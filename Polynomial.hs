module Polynomial where
    import Utils
    import Data.List as List
    import Data.Sequence as Seq
    import Data.Foldable
    import qualified Data.Text as Text
    data Polynomial = Polynomial {variable :: Variable, coefficients :: [Coefficient]} deriving (Show)
    type Coefficient = Int
    type Exponent = Int
    type Variable = String

    createPolynomial :: Polynomial
    createPolynomial = SimplePolynomial "y" [2,3]

    createPolynomial2 :: [Polynomial]
    createPolynomial2 = [SimplePolynomial "x" [3,5,7], CompositePolynomial "w^4*z^7" 2]

    polyVariable :: Polynomial -> String
    polyVariable = variable

    polyCoeffs :: Polynomial -> [Coefficient]
    polyCoeffs = coefficients

    polyFirstExp :: Polynomial -> Int
    polyFirstExp p = List.length (coefficients p)
    
    printPolynomial :: Variable -> Coefficient -> Coefficient -> String
    printPolynomial var coef exp
                    | coef == 0 = ""
                    | coef == 1 && List.null var = show coef
                    | coef == 1 && exp == 1 = var
                    | coef == 1 && exp /= 1 = var ++ "^" ++ show exp
                    | coef /= 1 && exp == 1 = show coef ++ "*" ++ var
                    | coef /= 1 && exp /= 1 = show coef ++ "*" ++ var ++ "^" ++ show exp
                    | List.length var > 1 = show coef
                    | otherwise = error "Error on printPolynomial"

    orderedPrint :: Variable -> Variable -> Exponent -> Exponent -> String
    orderedPrint v1 v2 e1 e2
                        | v1 < v2 = printPolynomial v1 1 e1 ++ "*" ++ printPolynomial v2 1 e2
                        | otherwise = printPolynomial v2 1 e2 ++ "*" ++ printPolynomial v1 1 e1

    sortCompositeVariables :: Polynomial -> Polynomial -> Ordering
    sortCompositeVariables p1 p2
                        | List.null (fst p1) = GT
                        | List.null (fst p2) = LT
                        | head (fst p1) < head (fst p2) = LT
                        | head (fst p1) > head (fst p2) = GT
                        | var_eq && single_first_var = LT
                        | var_eq && single_second_var = GT
                        | var_eq && single_vars = EQ
                        | var_eq && fst p1 !! 1 /= '^' && fst p2 !! 1 == '^' = GT
                        | var_eq && fst p2 !! 1 /= '^' && fst p1 !! 1 == '^' = LT
                        | parseFirstExp (fst p1) < parseFirstExp (fst p2) = GT
                        | parseFirstExp (fst p1) > parseFirstExp (fst p2) = LT
                        | List.length (fst p1) < List.length (fst p2) = LT
                        | List.length (fst p1) > List.length (fst p2) = GT
                        | otherwise = EQ
                        where single_vars = List.length (fst p1) == 1 && List.length (fst p2) == 1
                              single_first_var = List.length (fst p1) == 1 && List.length (fst p2) > 1
                              single_second_var = List.length (fst p1) > 1 && List.length (fst p2) == 1
                              var_eq = head (fst p1) == head (fst p2)

    polySplitOn :: String -> [String]
    polySplitOn s = List.sort (List.map Text.unpack (Text.splitOn (Text.pack "*") (Text.pack s)))


    showPolynomialTrimmed :: [Polynomial] -> String
    showPolynomialTrimmed (p:ps)
                            | head a == '-' = '-' : List.drop 2 a
                            | otherwise = a
                            where a = dropWhile toRemoveLeading (showPolynomial (p:ps))
    
    showPolynomial :: [Polynomial] -> String
    showPolynomial (p:ps)
                    | List.null (p:ps) = ""
                    | List.null (snd p) = showPolynomial ps
                    | last_monomial && head (snd p) < 0 = " - " ++ printPolynomial (fst p) (abs (head (snd p))) (List.length (snd p))
                    | last_monomial && head (snd p) == 0 = "" ++ printPolynomial (fst p) (head (snd p)) (List.length (snd p))
                    | last_monomial && head (snd p) > 0 = " + " ++ printPolynomial (fst p) (head (snd p)) (List.length (snd p))
                    | head (snd p) == 0 = printPolynomial (fst p) (head (snd p)) (List.length (snd p)) ++ showPolynomial ((fst p, tail (snd p)):ps)
                    | head (snd p) < 0 = " - " ++ printPolynomial (fst p) (abs (head (snd p))) (List.length (snd p)) ++ showPolynomial ((fst p, tail (snd p)):ps)
                    | otherwise = " + " ++ printPolynomial (fst p) (head (snd p)) (List.length (snd p)) ++ showPolynomial ((fst p, tail (snd p)):ps)
                    where last_monomial = List.null ps && List.length (snd p) == 1

    sortAndNormalize :: Polynomial -> Polynomial
    sortAndNormalize (p:ps)
                            | fst (head a) == "" = tail a ++ [head a]
                            | otherwise = a
                            where a = normalizePolynomial(List.sortBy sortCompositeVariables (p:ps))

    normalizePolynomial :: Polynomial -> Polynomial
    normalizePolynomial (p:ps)
                                | List.null ps && not (List.null (fst p)) = [p]
                                | List.length ps == 1 && fst p /= fst (head ps) = p:ps
                                | List.length ps == 1 && fst p == fst (head ps) = [(fst p, List.reverse (myListSum (List.reverse (snd p)) (List.reverse (snd (head ps)))))]
                                | List.null (fst p) && List.length (snd p) /= 1 && List.null ps = [(fst p, [List.sum (snd p)])]
                                | List.null (fst p) && List.length (snd p) /= 1 = normalizePolynomial ((fst p, [List.sum (snd p)]) : ps)
                                | fst p == fst (head ps) = normalizePolynomial ((fst p, List.reverse (myListSum (List.reverse (snd p)) (List.reverse (snd (head ps))))) : tail ps)
                                | otherwise = p : normalizePolynomial ps

    addPolynomial :: Polynomial -> Polynomial -> String
    addPolynomial a b = showPolynomialTrimmed (sortAndNormalize (a ++ b))

    multiplyPolynomials :: Polynomial -> Polynomial -> Polynomial
    multiplyPolynomials (p1:p1s) (p2:p2s)
                                        | List.null p1s = multPolyToList [p1] (p2:p2s)
                                        | otherwise = multPolyToList [p1] (p2:p2s) ++ multiplyPolynomials p1s (p2:p2s)

    multPolyToList :: Polynomial -> Polynomial -> Polynomial
    multPolyToList a (p:ps)
                            | List.null (fst p) && List.null ps = [(fst (head a), map (* head (snd p)) (snd (head a)))]
                            | List.null (fst p) = (fst (head a), map (* head (snd p)) (snd (head a))) : multPolyToList a ps
                            | List.null (fst (head a)) && List.null ps = [(fst p, map (* head (snd (head a))) (snd p))]
                            | List.null (fst (head a)) = (fst p, map (* head (snd (head a))) (snd p)) : multPolyToList a ps
                            | (List.length (fst p) > 1 || List.length (fst (head a)) > 1) && List.null ps = multCompositeVariables a [p]
                            | List.length (fst p) > 1 || List.length (fst (head a)) > 1 = multCompositeVariables a [p] ++ multPolyToList a ps
                            | fst (head a) == fst p && List.null ps = [(fst p, multSameVar (snd (head a)) (snd p) new_list)]
                            | fst (head a) == fst p = (fst p, multSameVar (snd (head a)) (snd p) new_list) : multPolyToList a ps
                            | fst (head a) /= fst p && List.null ps = multDiffVar a [p]
                            | fst (head a) /= fst p = multDiffVar a [p] ++ multPolyToList a ps
                            where new_list = List.replicate (List.length (snd (head a)) + List.length (snd p)) 0

    multSameVar :: [Coefficient] -> [Coefficient] -> [Coefficient] -> [Coefficient]
    multSameVar a b c
                        | List.null (tail a) = multSingleSameVar (head a) len b c
                        | otherwise = multSameVar (tail a) b (multSingleSameVar (head a) len b c)
                        where len = List.length a

    multSingleSameVar :: Coefficient -> Int -> [Coefficient] -> [Coefficient] -> [Coefficient]
    multSingleSameVar a l b c
                        | List.null (tail b) = toList (Seq.update idx new_coefficient (fromList c))
                        | otherwise = multSingleSameVar a l (tail b) (toList (Seq.update idx new_coefficient (fromList c)))
                        where idx = List.length c - l - List.length b; new_coefficient = a * head b + index (fromList c) idx

    multDiffVar :: Polynomial -> Polynomial -> Polynomial
    multDiffVar (p1:p1s) (p2:p2s)
                    | List.length (snd p1) == 1 = multSingleDiffVar (fst p1) (head (snd p1)) (List.length (snd p1)) [p2]
                    | otherwise = multSingleDiffVar (fst p1) (head (snd p1)) (List.length (snd p1)) [p2] ++ multDiffVar [(fst p1, tail (snd p1))] [p2]

    multSingleDiffVar :: Variable -> Coefficient -> Exponent -> Polynomial -> Polynomial
    multSingleDiffVar v c e (p:ps)
                    | List.length (snd p) == 1 = [(orderedPrint v (fst p) e (List.length (snd p)), [c * head (snd p)])]
                    | otherwise = (orderedPrint v (fst p) e (List.length (snd p)), [c * head (snd p)]) : multSingleDiffVar v c e [(fst p, tail (snd p))]

    multCompositeVariables :: Polynomial -> Polynomial -> Polynomial
    multCompositeVariables (p1:p1s) (p2:p2s)
                                            | List.length (snd p1) == 1 && List.length (fst p1) == 1 && List.length (fst p2) > 1 = iterate_first
                                            | List.length (snd p2) == 1 && List.length (fst p1) > 1 && List.length (fst p2) == 1 = iterate_second
                                            | List.length (snd p1) /= 1 && List.length (fst p1) == 1 && List.length (fst p2) > 1 = iterate_first ++ multCompositeVariables [(fst p1, tail (snd p1))] (p2:p2s)
                                            | List.length (snd p2) /= 1 && List.length (fst p1) > 1 && List.length (fst p2) == 1 = iterate_second ++ multCompositeVariables (p1:p1s) [(fst p2, tail (snd p2))]
                                            | otherwise = [(List.init (parseCompositeVariables (polySplitOn (fst p1 ++ "*" ++ fst p2))), [head (snd p1) * head (snd p2)])]
                                            where iterate_first = multSingleCompositeVariables (fst p1) (head (snd p1)) (List.length (snd p1)) [p2];
                                                    iterate_second = multSingleCompositeVariables (fst p2) (head (snd p2)) (List.length (snd p2)) [p1]

    multSingleCompositeVariables :: Variable -> Coefficient -> Exponent -> Polynomial -> Polynomial
    multSingleCompositeVariables v c e (p:ps) = [(List.init (parseCompositeVariables (polySplitOn (printPolynomial v 1 e ++ "*" ++ fst p))), [c * head (snd p)])]

    parseCompositeVariables :: [Variable] -> Variable
    parseCompositeVariables (p1:p1s)
                                        | List.null p1 = ""
                                        | List.null p1s = p1 ++ "*"
                                        | List.length p1s == 1 && head p1 == head (head p1s) = [head p1] ++ "^" ++ show ((read (parseFirstExp p1) :: Int) + (read (parseFirstExp (head p1s)) :: Int)) ++ "*"
                                        | head p1 == head (head p1s) = ([head p1] ++ "^" ++ show ((read (parseFirstExp p1) :: Int) + (read (parseFirstExp (head p1s)) :: Int))) ++ "*" ++ parseCompositeVariables (tail p1s)
                                        | otherwise = p1 ++ "*" ++ parseCompositeVariables p1s


    -- Differentiation Functions --

{-
  -- Main composite function for calculating and outputting derivatives -- TO SCRAP 
    deriveOp :: Polynomial -> String
    deriveOp = showPolynomial . sortAndNormalize . derivePolynomial
-}
    -- Main function for for calculating and outputting partial derivatives
    derivePartialOp :: Variable ->  [Polynomial] -> String
    derivePartialOp var p = showPolynomial(sortAndNormalize (derivePartial var p))
{-
    -- Differentiation function based on every variable present in the polynomial -- TO SCRAP
    derivePolynomial :: [Polynomial] -> [Polynomial]
    derivePolynomial [] = []
    derivePolynomial (x:xs) | variable x == "" = (fst x, [sum (coefficients x ++ [sumNewConstants (x:xs)])]) : derivePolynomial xs 
                            | otherwise = (fst x, derivePolyCoeffs (init (snd x)) 2) : derivePolynomial xs
-}                  

    -- Partial differentiation function based on 1 user-input variable
    derivePartial :: Variable -> [Polynomial] -> [Polynomial]
    derivePartial _ [] = []
    derivePartial var (x:xs) | variable x == "" = (variable x, [sum (coefficients x ++ [sumNewConstants var (x:xs)])]) : derivePartial var xs 
                            | variable x == var = (variable x, derivePolyCoeffs (init (coefficients x)) 2) : derivePartial var xs
                            | List.length (variable x) > 1 && isInfixOf var (variable x) = deriveCompositeVar (coefficients (head x)) x ++ derivePartial var xs
                            | otherwise = x : derivePartial var xs

    testderivePartial :: String
    testderivePartial = derivePartialOp "a" [("", [-1,6,-0,-4]), ("a^23*x", [-3]), ("y", [-1,-6,-0,-4]), ("x", [3]), ("z", [-1,-6,-0,-4])] 

    -- Function that returns value of new constants that appeared from differentiation on degree-one monomials
    sumNewConstants :: Variable -> [Polynomial] -> Int
    sumNewConstants var p = sum [x | x <- [last (coefficients i) | i <- p, (variable i) == var && List.length (variable i) < 1]]

    testSumNew :: Int
    testSumNew = sumNewConstants "w" [("", []), ("w", [-1,-6,-0,-4]), ("y", [-1,-6,-0,-4]), ("x", [-1,-6,-0,-4]), ("z", [-1,-6,-0,-4])]
                            
    -- Function that takes care of differentiation regarding composite variables
    deriveCompositeVar :: Char -> Polynomial -> [Polynomial]
    deriveCompositeVar var p = [getNewVarComp var parsed_vars (head (coefficients p))]
                            where parsed_vars = polySplitOn (variable p);
                                  selected_var = head [i | i <- parsed_vars, head i == var]


    getNewVarComp :: Char -> [String] -> Coefficient -> Polynomial
    getNewVarComp var s c | List.length selected_var == 1 = (concat (List.intersperse "" ([i | i <- s, (head i) /= var])), [c])
                          | List.length selected_var == 3 && selected_var !! 2 == '2' = (concat (List.intersperse "" ([i | i <- s, (head i) /= var] ++ ["*", [var]])), [c*2])
                          | otherwise = (concat (List.intersperse "" ([i | i <- s, (head i) /= var] ++ ["*", [var], "^"] ++ [show new_coefficient])), [c*(new_coefficient+1)])
                          where selected_var = head [i | i <- s, head i == var];
                                new_coefficient = lowerDegree (List.drop 2 selected_var)
                                        
    lowerDegree :: String -> Int
    lowerDegree n = (read n::Int) - 1 

    -- Recursive differentiation function that returns lists of coefficients
    derivePolyCoeffs :: [Coefficient] -> Int -> [Coefficient]
    derivePolyCoeffs [] n = []
    derivePolyCoeffs p 0 = derivePolyCoeffs (init p) 1 
    derivePolyCoeffs p n = derivePolyCoeffs (init p) (n+1) ++ [(last p) *n]

    testderiveCoeffs :: [Coefficient]
    testderiveCoeffs = derivePolyCoeffs ([1,4,8]) 0 