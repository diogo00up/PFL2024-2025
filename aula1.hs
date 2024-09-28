-- Function to check if three sides can form a valid triangle

testaTriangulo :: Float -> Float -> Float -> Bool
testaTriangulo a b c = a <  b + c && b < a + c && c < a + b

metades :: [a] -> ([a], [a])
metades lista = (take (length lista `div` 2) lista, drop (length lista `div` 2) lista)

last2 :: [a] -> a
last2 lista =  head(reverse lista)

last3 :: [a] -> a
last3 lista =   head (drop (length lista - 1) lista)

init2 :: [a] -> [a]
init2 lista = reverse(tail (reverse lista))

init3 :: [a] -> [a]
init3 lista =   reverse (drop 1 (reverse lista))

--exercice 7.1
-- [Char]
-- (Char,Char,Char)
-- [(Bool,String)]
-- Impossible
-- [a]
--
--exercice 8.1
-- [a] -> a
-- (a,b) -> (b,a)
-- a->b->(a,b)
-- Num a => a -> a
-- Fractional a => a -> a
-- Char -> Bool
-- Char -> Char -> Char -> Bool
-- [a] -> Bool
-- (a -> a) -> a -> a

xor :: Bool -> Bool -> Bool
xor True True = False
xor False False = False
xor True False = True
xor False True = True

------------------------------------------------------------------------
------------------------------------------------------------------------

unidades :: Int -> String
unidades 1 = "um"
unidades 2 = "dois"
unidades 3 = "três"
unidades 4 = "quatro"
unidades 5 = "cinco"
unidades 6 = "seis"
unidades 7 = "sete"
unidades 8 = "oito"
unidades 9 = "nove"
unidades _ = ""

dezenasEspeciais :: Int -> String
dezenasEspeciais 10 = "dez"
dezenasEspeciais 11 = "onze"
dezenasEspeciais 12 = "doze"
dezenasEspeciais 13 = "treze"
dezenasEspeciais 14 = "catorze"
dezenasEspeciais 15 = "quinze"
dezenasEspeciais 16 = "dezesseis"
dezenasEspeciais 17 = "dezessete"
dezenasEspeciais 18 = "dezoito"
dezenasEspeciais 19 = "dezenove"
dezenasEspeciais _  = ""

dezenas :: Int -> String
dezenas 2 = "vinte"
dezenas 3 = "trinta"
dezenas 4 = "quarenta"
dezenas 5 = "cinquenta"
dezenas 6 = "sessenta"
dezenas 7 = "setenta"
dezenas 8 = "oitenta"
dezenas 9 = "noventa"
dezenas _ = ""

menorQue100 :: Int -> String
menorQue100 numero
    | numero < 10 = unidades numero
    | numero < 20 = dezenasEspeciais numero
    | numero < 100 = dezenas( numero `div` 10 ) ++ if (numero `mod` 10 /= 0) then " e " ++ unidades(numero `mod` 10) else ""

centenas :: Int -> String
centenas 1 = "cento"
centenas 2 = "duzentos"
centenas 3 = "trezentos"
centenas 4 = "quatrocentos"
centenas 5 = "quinhentos"
centenas 6 = "seiscentos"
centenas 7 = "setecentos"
centenas 8 = "oitocentos"
centenas 9 = "novecentos"
centenas _ = ""

menorQue1000 :: Int -> String
menorQue1000 numero 
    | numero == 100 = "cem"
    | numero < 1000 = centenas( numero `div` 100 ) ++ if numero `mod` 100 /= 0 then " e " ++ menorQue100 (numero `mod` 100) else ""

milhares :: Int -> String
milhares n
  | n == 1000 = "mil"
  | n < 1000    = menorQue1000 n
  | n < 1000000 = let mil = n `div` 1000
                    resto = n `mod` 1000
                  in if mil == 1
                     then "mil" ++ if resto /= 0 then " " ++ menorQue1000 resto else ""
                     else menorQue1000 mil ++ " mil" ++ if resto /= 0 then " " ++ menorQue1000 resto else ""

converte :: Int -> String
converte n
  | n >= 1000000 = error "Número fora do limite (deve ser menor que 1 milhão)"
  | otherwise    = milhares n
