filtrarGuionISBN :: String -> String
filtrarGuionISBN x = filter (\x -> x /= '-') x

transformarUltimoCaracter :: String -> [String]
transformarUltimoCaracter x =
  if last x == 'X' then map (\x -> if x == 'X' then "10" else [x]) x
  else map(\x -> [x]) x

operacionesISBN :: [String] -> Int
operacionesISBN x =
  if length x == 0 then 0
  else read (head x) * length x + operacionesISBN (tail x)

validarISBN :: Int -> Bool
validarISBN x =
  mod x 11 == 0
 
main :: IO ()
main = do
  print(filtrarGuionISBN "12-34567-89X")
  print(transformarUltimoCaracter (filtrarGuionISBN "12-34567-89X"))
  print(operacionesISBN(transformarUltimoCaracter (filtrarGuionISBN "12-34567-89X")))
  print(validarISBN (operacionesISBN(transformarUltimoCaracter (filtrarGuionISBN "12-34567-89X"))))
  print(validarISBN (operacionesISBN(transformarUltimoCaracter (filtrarGuionISBN "0198526636"))))
  print(validarISBN (operacionesISBN(transformarUltimoCaracter (filtrarGuionISBN "019852663X"))))
