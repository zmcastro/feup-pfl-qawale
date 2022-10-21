module Polynomial where
    type Polynomial = [(Variable, [Coefficient])]
    type Coefficient = Int
    type Exponent = Int
    type Variable = String

    createPoly :: Variable -> Coefficient -> Polynomial
    createPoly var coeff = [(var, [coeff])]
    
    concatPoly :: Polynomial -> Polynomial -> Polynomial
    concatPoly p1 p2 = p1 ++ p2

    {-
    findPoly :: Variable -> Polynomial -> (Variable, [Coefficient])
    findPoly var p = if (null index) then () else poly 
        where { poly = [p | p <- p, var == (fst poly)] }
    -}

    -- find function to check existence and find variable inside of polynomial
    -- returns list index of variable match if it exists, else returns -1 
    findPolyIdx :: Variable -> Polynomial -> Int
    findPolyIdx var p = if (null index) then -1 else (head index) 
        where { index = [i | (i, poly) <- zip [0..] p, var == (fst poly)] }

    testFind :: Int 
    testFind = findPolyIdx "x" [("y", [1,4,8]), ("a", [2,4])]
