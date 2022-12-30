module Polynomial where
      import Utils
      import Data.Char
      import Data.List as List
      import Data.Sequence as Seq
      import Data.Foldable
      
      data Polynomial = Polynomial {variable :: Variable, coefficients :: [Coefficient]} deriving (Show)
      type Coefficient = Int
      type Exponent = Int
      type Variable = String


      {- Polynomial-oriented utility functions -}


      -- Returns the exponent of the current monomial (the coefficient list's length is equal to the exponent of 
      -- the monomial that the first coefficient in the list corresponds to)
      getExponent :: Polynomial -> Int
      getExponent p = List.length (coefficients p)

      -- Parses the first exponent of a composite variable
      parseFirstExp :: String -> String
      parseFirstExp a = do    
                        let result = takeWhile isDigit (dropWhile (not . isDigit) a)
                        case result of 
                              "" -> "1"
                              e -> e

      -- Orders the way composite variables are displayed in the polynomial, it helps in the normalization.
      orderedPrint :: Variable -> Variable -> Exponent -> Exponent -> String
      orderedPrint v1 v2 e1 e2
                        | v1 < v2 = printPolynomial v1 1 e1 ++ "*" ++ printPolynomial v2 1 e2
                        | otherwise = printPolynomial v2 1 e2 ++ "*" ++ printPolynomial v1 1 e1

      -- Custom sort function that organizes a given polynomial: ascending order of variables and descending order of the (first) exponent.
      -- The constants get placed last manually because the empty string comes first in a regular sort.
      sortCompositeVariables :: Polynomial -> Polynomial -> Ordering
      sortCompositeVariables p1 p2
                        | List.null (variable p1) = GT
                        | List.null (variable p2) = LT
                        | head (variable p1) < head (variable p2) = LT
                        | head (variable p1) > head (variable p2) = GT
                        | var_eq && single_first_var = LT
                        | var_eq && single_second_var = GT
                        | var_eq && single_vars = EQ
                        | var_eq && variable p1 !! 1 /= '^' && variable p2 !! 1 == '^' = GT
                        | var_eq && variable p2 !! 1 /= '^' && variable p1 !! 1 == '^' = LT
                        | parseFirstExp (variable p1) < parseFirstExp (variable p2) = GT
                        | parseFirstExp (variable p1) > parseFirstExp (variable p2) = LT
                        | List.length (variable p1) < List.length (variable p2) = LT
                        | List.length (variable p1) > List.length (variable p2) = GT
                        | otherwise = EQ
                        where single_vars = List.length (variable p1) == 1 && List.length (variable p2) == 1
                              single_first_var = List.length (variable p1) == 1 && List.length (variable p2) > 1
                              single_second_var = List.length (variable p1) > 1 && List.length (variable p2) == 1
                              var_eq = head (variable p1) == head (variable p2)


      {- Print/normalization functions -}


      -- Removes any leading unwanted symbols returned by the showPolynomial function (as specified by the function toRemoveLeading from the Utils module).
      showPolynomialTrimmed :: [Polynomial] -> String
      showPolynomialTrimmed [] = "0"
      showPolynomialTrimmed (p:ps)
                        | List.null a = "0" 
                        | head a == '-' = '-' : List.drop 2 a
                        | otherwise = a
                        where a = dropWhile toRemoveLeading (showPolynomial (p:ps))

      -- Displays a polynomial in a user-friendly way.
      showPolynomial :: [Polynomial] -> String
      showPolynomial [] = []
      showPolynomial (p:ps)
                        | List.null (coefficients p) = showPolynomial ps
                        | last_monomial && head (coefficients p) < 0 = " - " ++ printPolynomial (variable p) (abs (head (coefficients p))) (getExponent p)
                        | last_monomial && head (coefficients p) == 0 = "" ++ printPolynomial (variable p) (head (coefficients p)) (getExponent p)
                        | last_monomial && head (coefficients p) > 0 = " + " ++ printPolynomial (variable p) (head (coefficients p)) (getExponent p)
                        | head (coefficients p) == 0 = printPolynomial (variable p) (head (coefficients p)) (getExponent p) ++ showPolynomial (Polynomial (variable p) (tail (coefficients p)):ps)
                        | head (coefficients p) < 0 = " - " ++ printPolynomial (variable p) (abs (head (coefficients p))) (getExponent p) ++ showPolynomial (Polynomial (variable p) (tail (coefficients p)):ps)
                        | otherwise = " + " ++ printPolynomial (variable p) (head (coefficients p)) (getExponent p) ++ showPolynomial (Polynomial (variable p) (tail (coefficients p)):ps)
                        where last_monomial = List.null ps && getExponent p == 1

      -- Sorts a polynomial by the sortCompositeVariables function and then normalizes it.
      sortAndNormalize :: [Polynomial] -> [Polynomial]
      sortAndNormalize (p:ps) = normalizePolynomial(List.sortBy sortCompositeVariables (p:ps))

      -- WARNING: this function does not work as intended if the polynomial is not sorted and never gets called on its own: always through sortAndNormalize.
      -- Normalizes a given polynomial as long as it's ordered (sortAndNormalize does that and then calls this function).
      normalizePolynomial :: [Polynomial] -> [Polynomial]
      normalizePolynomial [] = []
      normalizePolynomial (p:ps)
                        | List.length ps == 1 && variable p == variable (head ps) && List.null (variable p) = [Polynomial (variable p) [List.sum (coefficients p ++ coefficients (head ps))]]
                        | List.length ps == 1 && variable p == variable (head ps) = [Polynomial (variable p) (List.reverse (myListZipSum (List.reverse (coefficients p)) (List.reverse (coefficients (head ps)))))]
                        | List.null (variable p) && List.length (coefficients p) /= 1 && List.null ps = [Polynomial (variable p) [List.sum (coefficients p)]]
                        | List.null (variable p) && List.length (coefficients p) /= 1 = normalizePolynomial (Polynomial (variable p) [List.sum (coefficients p)] : ps)
                        | List.null ps = [p]
                        | variable p == variable (head ps) = normalizePolynomial (Polynomial (variable p) (List.reverse (myListZipSum (List.reverse (coefficients p)) (List.reverse (coefficients (head ps))))) : tail ps)
                        | otherwise = p : normalizePolynomial ps

      -- Handles the display of a given monomial
      printPolynomial :: Variable -> Coefficient -> Exponent -> String
      printPolynomial var coef exp
                        | exp < 0 = error "Negative exponent!"
                        | coef == 0 && var == "" = show coef
                        | coef == 0 = ""
                        | List.null var = show coef
                        | coef == 1 && List.null var = show coef
                        | coef == 1 && exp == 1 = var
                        | coef == 1 && exp /= 1 = var ++ "^" ++ show exp
                        | coef /= 1 && exp == 1 = show coef ++ "*" ++ var
                        | coef /= 1 && exp /= 1 = show coef ++ "*" ++ var ++ "^" ++ show exp
                        | List.length var > 1 = show coef
                        | otherwise = error "Error on printPolynomial"


      {- Addition -}


      -- Adds two polynomials together.
      addPolynomial :: [Polynomial] -> [Polynomial] -> [Polynomial]
      addPolynomial a b = sortAndNormalize (a ++ b)

      -- Calls addPolynomial and displays the result in a user-friendly way.
      showAddPolynomial :: [Polynomial] -> [Polynomial] -> String
      showAddPolynomial a b = showPolynomialTrimmed (addPolynomial a b)


      {- Multiplication -}


      -- Multiplies two polynomials.
      multiplyPolynomial :: [Polynomial] -> [Polynomial] -> [Polynomial]
      multiplyPolynomial (p1:p1s) (p2:p2s)
                        | List.null p1s = multPolyToList p1 (p2:p2s)
                        | otherwise = multPolyToList p1 (p2:p2s) ++ multiplyPolynomial p1s (p2:p2s)

      -- Calls multiplyPolynomial and displays the result in a user-friendly way.
      showMultiplyPolynomial :: [Polynomial] -> [Polynomial] -> String
      showMultiplyPolynomial a b = showPolynomialTrimmed (sortAndNormalize (multiplyPolynomial a b))

      -- Multiplies one polynomial portion to a polynomial.
      multPolyToList :: Polynomial -> [Polynomial] -> [Polynomial]
      multPolyToList a (p:ps)
                        | List.null (variable p) && List.null ps = [Polynomial (variable a) (map (* head (coefficients p)) (coefficients a))]
                        | List.null (variable p) = Polynomial (variable a) (map (* head (coefficients p)) (coefficients a)) : multPolyToList a ps
                        | List.null (variable a) && List.null ps = [Polynomial (variable p) (map (* head (coefficients a)) (coefficients p))]
                        | List.null (variable a) = Polynomial (variable p) (map (* head (coefficients a)) (coefficients p)) : multPolyToList a ps
                        | (List.length (variable p) > 1 || List.length (variable a) > 1) && List.null ps = multCompositeVariables a p
                        | List.length (variable p) > 1 || List.length (variable a) > 1 = multCompositeVariables a p ++ multPolyToList a ps
                        | variable a == variable p && List.null ps = [Polynomial (variable p) (multSameVar (coefficients a) (coefficients p) new_list)]
                        | variable a == variable p = Polynomial (variable p) (multSameVar (coefficients a) (coefficients p) new_list) : multPolyToList a ps
                        | variable a /= variable p && List.null ps = multDiffVar a p
                        | variable a /= variable p = multDiffVar a p ++ multPolyToList a ps
                        where new_list = List.replicate (List.length (coefficients a) + List.length (coefficients p)) 0

      -- Multiplies the list of coefficients of two polynomial portions with the same variable.
      -- Since the exponent of a monomial is equal to the length of the coefficient list when its coefficient is at the head, the length is also passed to the next function.
      multSameVar :: [Coefficient] -> [Coefficient] -> [Coefficient] -> [Coefficient]
      multSameVar a b c
                        | List.null (tail a) = multSingleSameVar (head a) len b c
                        | otherwise = multSameVar (tail a) b (multSingleSameVar (head a) len b c)
                        where len = List.length a

      -- Multiplies one of the coefficients of the first polynomial portion to the other coefficient list.
      -- "l" serves as the exponent of the monomial with the coefficient "a". Sequences are used to update the new coefficient list through direct indexing.
      multSingleSameVar :: Coefficient -> Int -> [Coefficient] -> [Coefficient] -> [Coefficient]
      multSingleSameVar a l b c
                        | List.null (tail b) = toList (Seq.update idx new_coefficient (fromList c))
                        | otherwise = multSingleSameVar a l (tail b) (toList (Seq.update idx new_coefficient (fromList c)))
                        where idx = List.length c - l - List.length b; new_coefficient = a * head b + index (fromList c) idx

      -- Multiplies two polynomial portions with different variables.
      multDiffVar :: Polynomial -> Polynomial -> [Polynomial]
      multDiffVar p1 p2
                        | getExponent p1 == 1 = multSingleDiffVar (Polynomial (variable p1) [head (coefficients p1)]) p2
                        | otherwise = multSingleDiffVar (Polynomial (variable p1) (coefficients p1)) p2 ++ multDiffVar (Polynomial (variable p1) (tail (coefficients p1))) p2

      -- Multiplies a monomial with a polynomial portion of a different variable.
      multSingleDiffVar :: Polynomial -> Polynomial -> [Polynomial]
      multSingleDiffVar p1 p2
                        | getExponent p2 == 1 = [complex_polynomial]
                        | otherwise = complex_polynomial : multSingleDiffVar p1 (Polynomial (variable p2) (tail (coefficients p2)))
                        where complex_polynomial = Polynomial (orderedPrint (variable p1) (variable p2) (getExponent p1) (getExponent p2)) [head (coefficients p1) * head (coefficients p2)]

      -- Multiplies polynomial portions where at least one of them is a composite variable monomial ("x*y^2", for example)
      multCompositeVariables :: Polynomial -> Polynomial -> [Polynomial]
      multCompositeVariables p1 p2
                        | List.length (coefficients p1) == 1 && List.length (variable p1) == 1 && List.length (variable p2) > 1 = iterate_first
                        | List.length (coefficients p2) == 1 && List.length (variable p1) > 1 && List.length (variable p2) == 1 = iterate_second
                        | List.length (coefficients p1) /= 1 && List.length (variable p1) == 1 && List.length (variable p2) > 1 = iterate_first ++ multCompositeVariables (Polynomial (variable p1) (tail (coefficients p1))) p2
                        | List.length (coefficients p2) /= 1 && List.length (variable p1) > 1 && List.length (variable p2) == 1 = iterate_second ++ multCompositeVariables p1 (Polynomial (variable p2) (tail (coefficients p2)))
                        | otherwise = [Polynomial (List.init (parseCompositeVariables (polySplitOn (variable p1 ++ "*" ++ variable p2)))) [head (coefficients p1) * head (coefficients p2)]]
                        where iterate_first = [multSingleCompositeVariables p1 p2];
                              iterate_second = [multSingleCompositeVariables p2 p1]

      -- Multiplies one single variable monomial with a composite variable monomial.
      multSingleCompositeVariables :: Polynomial -> Polynomial -> Polynomial
      multSingleCompositeVariables p1 p2 = Polynomial (List.init (parseCompositeVariables (polySplitOn (printPolynomial (variable p1) 1 (getExponent p1) ++ "*" ++ variable p2)))) [head (coefficients p1) * head (coefficients p2)]

      -- Parses the exponents and literals of a composite variable monomial to facilitate multiplication.
      parseCompositeVariables :: [Variable] -> Variable
      parseCompositeVariables (p1:p1s)
                        | List.null p1 = ""
                        | List.null p1s = p1 ++ "*"
                        | List.length p1s == 1 && head p1 == head (head p1s) = [head p1] ++ "^" ++ show ((read (parseFirstExp p1) :: Int) + (read (parseFirstExp (head p1s)) :: Int)) ++ "*"
                        | head p1 == head (head p1s) = ([head p1] ++ "^" ++ show ((read (parseFirstExp p1) :: Int) + (read (parseFirstExp (head p1s)) :: Int))) ++ "*" ++ parseCompositeVariables (tail p1s)
                        | otherwise = p1 ++ "*" ++ parseCompositeVariables p1s

      -- Differentiation --

      -- Calls multiplyPolynomial and displays the result in a user-friendly way.
      showDerivePartial :: Variable -> [Polynomial] -> String
      showDerivePartial deriving_var p = showPolynomialTrimmed (sortAndNormalize (derivePartial deriving_var p))        

      -- Calls function for partial differentiation function based on a simple variable and treats constants separately
      derivePartial :: Variable -> [Polynomial] -> [Polynomial]
      derivePartial deriving_var (x:xs) = if new_constants == 0
            then  derivePartialNoConstants deriving_var (x:xs) 
            else  derivePartialNoConstants deriving_var (x:xs) ++ [Polynomial "" [new_constants]]
            where { new_constants = sumNewConstants deriving_var (x:xs);}
      
      -- Derivation of literals with no addition of constants
      derivePartialNoConstants :: Variable -> [Polynomial] -> [Polynomial]
      derivePartialNoConstants deriving_var [] = []
      derivePartialNoConstants deriving_var (x:xs)  
                        | variable (last (x:xs)) == "" = derivePartialNoConstants deriving_var (last xs : (x: init xs))
                        | variable x == deriving_var = Polynomial (variable x) (derivePolyCoeffs (init (coefficients x)) 2) : derivePartialNoConstants deriving_var xs
                        | List.length (variable x) > 1 && isInfixOf deriving_var (variable x) = deriveCompositeVar (head deriving_var) x ++ derivePartialNoConstants deriving_var xs
                        | otherwise = derivePartialNoConstants deriving_var xs

      -- Differentiation regarding composite variables
      deriveCompositeVar :: Char -> Polynomial -> [Polynomial]
      deriveCompositeVar deriving_var p = [getNewVarComp deriving_var parsed_vars (head (coefficients p))]
                              where parsed_vars = polySplitOn (variable p);
                                    selected_var = head [i | i <- parsed_vars, head i == deriving_var]

      -- Builds the updated composite variable after diferentiation.
      getNewVarComp :: Char -> [String] -> Coefficient -> Polynomial
      getNewVarComp deriving_var s c 
                        | List.length selected_var == 1 = Polynomial (intercalate "" ([i | i <- s, head i /= deriving_var])) [c]
                        | List.length selected_var == 3 && selected_var !! 2 == '2' = Polynomial (intercalate "" ([i | i <- s, head i /= deriving_var] ++ ["*", [deriving_var]])) [c*2]
                        | otherwise = Polynomial (intercalate "" ([i | i <- s, head i /= deriving_var] ++ ["*", [deriving_var], "^"] ++ [show new_coefficient])) [c*(new_coefficient+1)]
                        where selected_var = head [i | i <- s, head i == deriving_var];
                              new_coefficient = lowerDegree (List.drop 2 selected_var)

      -- Returns value of new constants that appeared from differentiation on degree-one monomials
      sumNewConstants :: Variable -> [Polynomial] -> Int
      sumNewConstants deriving_var p = sum [last (coefficients i) | i <- p, variable i == deriving_var]

      -- Returns lists of coefficients after differentiation.
      derivePolyCoeffs :: [Coefficient] -> Int -> [Coefficient]
      derivePolyCoeffs [] n = [0]
      derivePolyCoeffs p 0 = derivePolyCoeffs (init p) 1 
      derivePolyCoeffs p n = derivePolyCoeffs (init p) (n+1) ++ [last p *n] 
