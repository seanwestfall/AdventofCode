splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn _ [] = []
splitOn f l@(x:xs)
    | f x = splitOn f xs
    | otherwise = let (h,t) = break f l in h:(splitOn f t)

{--
intCode :: [int] -> [int]
intCode n = [0]
--}

main :: IO ()
main = do
    content <- readFile "02/02.txt"
    let opCodes = splitOn (== ',') content
    print opCodes



