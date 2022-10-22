module Tests where

    testDerivePartial :: String
    testDerivePartial = derivePartialOp "y" [Polynomial "" [8], Polynomial "a^23*x" [-3], Polynomial "y" [-1,-6,-0,-4], Polynomial "x" [3], Polynomial "z" [-1,-6,-0,-4]] 

    testSumNewConstants :: Int
    testSumNewConstants = sumNewConstants "w" [Polynomial "" [2], Polynomial "w" [-1,-6,-0,-4], Polynomial "y" [-1,-6,-0,-4], Polynomial "x" [-1,-6,-0,-4], Polynomial "z" [-1,-6,-0,-4]]
    
    testDeriveCoeffs :: [Coefficient]
    testDeriveCoeffs = derivePolyCoeffs [2,4,8] 0