import Data.List

condNumero :: Float -> Float
condNumero x 
  | x == 3 = 100
  | x == 5 = 25
  | otherwise = 0

solveRPN :: String -> Float  
solveRPN = head . foldl foldingFunction [] . words
  where   foldingFunction (x:y:ys) "+" = (x + y):ys
          foldingFunction (x:y:ys) "-" = (y - x):ys
          foldingFunction (x:y:ys) "*" = (x * y):ys
          foldingFunction (x:y:ys) "/" = (y / x):ys
          foldingFunction (x:xs) "neg1" = (negate x): xs
          foldingFunction (x:xs) "sqrt" = (sqrt x) : xs
          foldingFunction (x:xs) "condNumero" = (condNumero x):xs
          foldingFunction xs "sumaTotal" = [sum xs]
          foldingFunction xs "producto" = [product xs]
          foldingFunction xs "promedio" = (sum xs / fromIntegral (length xs)) :xs
          foldingFunction xs numberString = read numberString:xs

main = do
  print(solveRPN "1 2 3 4 5 promedio")
  print(solveRPN "2 2 + sqrt")
  print(solveRPN "1 2 3 4 5 producto")
  print(solveRPN "5 8 + 14 - neg1")