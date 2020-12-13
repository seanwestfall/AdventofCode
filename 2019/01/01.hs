readInts :: String -> [Int]
readInts s = let ws = words s in map read ws

getFuel :: Int -> Int
getFuel n = ( n `div` 3 ) - 2

getFuel2 :: Int -> Int
getFuel2 n
    | n <= 0    = 0
    | otherwise = n + getFuel2 ( getFuel n )

main :: IO ()
main = do
    content <- readFile "01.txt"
    let values = readInts content
        newValues = map getFuel values
        newValues2 = map getFuel2 newValues
        total = sum newValues
        total2 = sum newValues2
    putStrLn $ "Answer Part 1 : " ++ show total
    putStrLn $ "Answer Part 2 : " ++ show total2


