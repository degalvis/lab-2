import Data.List

solveRPN :: String -> Float
solveRPN = head . foldl foldingFunction [] . words

  where 
    foldingFunction (x:y:ys) "*" = (x * y):ys
    foldingFunction (x:y:ys) "+" = (x + y):ys
    foldingFunction (x:y:ys) "-" = (y - x):ys
    foldingFunction (x:y:ys) "/" = (y / x):ys
    foldingFunction (x:xs) "neg1" = (x - x*2): xs
    foldingFunction (x:xs) "sqrt" = (sqrt x) : xs
    foldingFuction (x:xs) "condNumero" = (condNumero x):xs
    foldingFunction xs "sum" = [sum xs]
    foldingFunction xs numberString = read numberString:xs


