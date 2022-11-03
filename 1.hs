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
          foldingFunction (x:xs) "promedio" = (sum xs / fromIntegral (length xs)) :xs
          foldingFunction xs numberString = read numberString:xs

main :: IO()
main = return ()