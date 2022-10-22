# PFL - 1º Trabalho Prático 

## Representação

A representação interna de um polinómio no nosso trabalho é declarada da seguinte forma:

    data Polynomial = Polynomial {variable :: Variable, coefficients :: [Coefficient]} deriving (Show)

    type Coefficient = Int
    type Exponent = Int
    type Variable = String

e consiste na divisão semântica de cada parcela importante de um polinómio de maneira concisa: um parâmetro *variable* que guarda os variáveis literais de um polinómio/monómio e uma lista *coefficients* que guarda coeficientes associados à variável usando o index na lista como representação do expoente, de forma ascendente e começando no primeiro grau.
Tendo um polinómio que consta com mais do que uma variável, cada polinómio associado a uma variável específica é guardada numa super-lista de polinómios. 

Qualquer constante é guardada numa estrutura com o mesmo nome cuja variável é uma string vazia ("") e cuja lista de *coefficients* conserva apenas as constantes que aparecem no polinómio.

Nos edge cases em que um polinómio apresenta variáveis compostas (ex.: "a*x^2"), esta é tratada de forma diferente: a representação em polinómio agrega tanto variáveis e expoentes e a lista de coeficientes resume-se a apenas o mesmo relativo a essa variável.

Exemplificando:
#   
    "2*x^3 + 5*x^2" => ("x", [2, 5, 0, 0])
    -
    "x^4 + 5*y + 10 + 2 => [("", [10, 2]), ("x", [4, 0, 0, 0]), [("y", [5])]]
    -
    "4*x*y^2" => ("x*y", [4])
