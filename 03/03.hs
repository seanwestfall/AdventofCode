
main :: IO ()
main = do
    content <- readFile "03.txt"
    putStrLn $ content
