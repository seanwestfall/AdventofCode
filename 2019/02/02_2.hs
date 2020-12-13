#! /usr/bin/env -S"ANSWER=42" nix-shell
#! nix-shell -p ghcid
#! nix-shell -p "haskellPackages.ghcWithPackages (pkgs: [pkgs.lens])"
#! nix-shell -i "ghcid -T main"

import Control.Monad

opCode :: [Int] -> Int -> [Int]
opCode list c = case (list!!c) of
                     1  -> opCode ( replaceNth (list!!(c+3)) ( (list!!(list!!(c+1))) + (list!!(list!!(c+2))) ) list ) (c + 4)
                     2  -> opCode ( replaceNth (list!!(c+3)) ( (list!!(list!!(c+1))) * (list!!(list!!(c+2))) ) list ) (c + 4)
                     99 -> list

splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn _ [] = []
splitOn f l@(x:xs)
    | f x = splitOn f xs
    | otherwise = let (h,t) = break f l in h:(splitOn f t)

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
    | n == 0 = newVal:xs
    | otherwise = x:replaceNth (n-1) newVal xs

reverseList :: [Int] -> [Int]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

main :: IO ()
main = do
    content <- readFile "02.txt"
    let opCodes   = splitOn (== ',') content
        newCodes  = map (read::String->Int) opCodes
        list      = replaceNth 1 12 newCodes
        list2     = replaceNth 2 2 list 
        finallist = opCode list2 0
    -- putStrLn $ "input List: " ++ show list2
    -- putStrLn $ "return list: " ++ show finallist
    putStrLn $ "Answer One: The value at position 0 is " ++ show (finallist!!0)
    let input = 19690720
    forM_ [0..99] $ \i -> do
        forM_ [0..99] $ \j -> do
            let sndList1 = replaceNth 1 i list2
                sndList2 = replaceNth 2 j sndList1
                sndAns   = opCode sndList2 0
            when (sndAns!!0 == input) $ putStrLn $ show sndList2 ++ " " ++ show i ++ " " ++ show j
    -- let test      = opCode [2,1,5,0,99,19690720] 0
    -- putStrLn $ "test: " ++ show (test!!0)
    -- putStrLn $ show $ reverseList finallist
    let test = opCode [1,84,44,3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,10,19,1,19,5,23,2,23,9,27,1,5,27,31,1,9,31,35,1,35,10,39,2,13,39,43,1,43,9,47,1,47,9,51,1,6,51,55,1,13,55,59,1,59,13,63,1,13,63,67,1,6,67,71,1,71,13,75,2,10,75,79,1,13,79,83,1,83,10,87,2,9,87,91,1,6,91,95,1,9,95,99,2,99,10,103,1,103,5,107,2,6,107,111,1,111,6,115,1,9,115,119,1,9,119,123,2,10,123,127,1,127,5,131,2,6,131,135,1,135,5,139,1,9,139,143,2,143,13,147,1,9,147,151,1,151,2,155,1,9,155,0,99,2,0,14,0] 0
    putStrLn $ show (test!!0)
    putStrLn "finished"

