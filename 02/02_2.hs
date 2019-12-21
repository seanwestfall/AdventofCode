import Control.Monad
import Control.Monad.State
 
-- Takes two lists, the list to be interated, and the second is the
-- in place temporary list that with take in edits.
{--
opCode :: [Int] -> [Int] -> [Int]
opCode list temp = do
    let index = 0
    forM_ list $ \i -> do
        print i
        print index
        index + 1
--}



main :: IO ()
main = do
    -- content <- readFile "02/02.txt"
    (on, score) <- get
    let list = [1,9,10,3,2,3,11,0,99,30,40,50]
        index = 0
    forM_ list $ \i -> do
        putStrLn $ show i
        put (on, score + 1)
        putStrLn $ show index
    putStrLn "finished"


main = do
    let list = [1,9,10,3,2,3,11,0,99,30,40,50]
    let loop list = do
        i < get
        put (i + 1)
        forM_ list $ \n -> do
            putStrLn $ show n
            put (i + 1)
            putStrLn $ show i
     in \s -> evalStateT (loop list) 0
            
