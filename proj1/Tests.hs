module Tests where
    import Polynomial
    import Utils

    {- Test functions -}
    
    -- Example polynomials that are used by the tests. Modify them as you wish.
    
    testPolynomial :: [Polynomial]
    testPolynomial = [Polynomial "x" [5,5], Polynomial "y" [10], Polynomial "" [2]]

    testPolynomial2 :: [Polynomial]
    testPolynomial2 = [Polynomial "z*w" [3], Polynomial "y" [2,3], Polynomial "" [5]]

    testMonomial :: Polynomial
    testMonomial = Polynomial "x" [3]

    -- Set of tests that call the main operation functions or functions used by them separately, in order to expose the inner workings of the program.

    testNormalize :: [Polynomial]
    testNormalize = sortAndNormalize testPolynomial

    -- The function correspondant to this test is ran recursively on a Polynomial. To test different exponents, add more coefficients to testMonomial or hardcode them here.
    testPrintPolynomial :: String
    testPrintPolynomial = printPolynomial (variable testMonomial) (head (coefficients testMonomial)) (getExponent testMonomial)

    --This test is not a direct test of showPolynomial - but rather a test on how the user would see a Polynomial after all the conversions.
    testShowPolynomial :: String
    testShowPolynomial = showPolynomialTrimmed (sortAndNormalize testPolynomial)
    
    testAddPolynomial :: [Polynomial]
    testAddPolynomial = addPolynomial testPolynomial testPolynomial2

    testMultiplyPolynomial :: [Polynomial]
    testMultiplyPolynomial = multiplyPolynomial testPolynomial testPolynomial2

    testDerivePartial :: [Polynomial]
    testDerivePartial = derivePartial "y" testPolynomial

    testSumNewConstants :: Int
    testSumNewConstants = sumNewConstants "y" testPolynomial
    
    testDeriveCoeffs :: [Coefficient]
    testDeriveCoeffs = derivePolyCoeffs [2,4,8] 0