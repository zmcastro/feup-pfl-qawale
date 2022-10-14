module Polynomial where
    type Polynomial = [(Variable, [Coefficient])]
    type Coefficient = Int
    type Exponent = Int
    type Variable = String

    createPoly :: Variable -> Coefficient -> Polynomial
    createPoly var coeff = [(var, [coeff])]
    
    concatPoly :: Polynomial -> Polynomial -> Polynomial
    concatPoly p1 p2 = p1 ++ p2