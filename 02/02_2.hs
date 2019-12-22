#! /usr/bin/env -S"ANSWER=42" nix-shell
#! nix-shell -p ghcid
#! nix-shell -p "haskellPackages.ghcWithPackages (pkgs: [pkgs.lens])"
#! nix-shell -i "ghcid -T main"

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

main :: IO ()
main = do
    content <- readFile "02_2.txt"
    let opCodes   = splitOn (== ',') content
        newCodes  = map (read::String->Int) opCodes
        list      = replaceNth 1 12 newCodes
        list2     = replaceNth 2 2 list 
        finallist = opCode list2 0
    putStrLn $ show finallist
    putStrLn $ "The value at position 0 is " ++ show (finallist!!0)

