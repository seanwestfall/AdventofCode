
main :: IO ()
main = do
    content <- readFile "03.txt"
    -- create the manhattan distance
    -- find where the wires cross
    -- compute the distance
    let n = [(x, y)| x<- [0..10], y<-[0..10]]
    putStrLn $ content
    mapM_ print n
