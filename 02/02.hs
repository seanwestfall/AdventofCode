#! /usr/bin/env -S"ANSWER=42" nix-shell
#! nix-shell -p ghcid
#! nix-shell -p "haskellPackages.ghcWithPackages (pkgs: [pkgs.lens])"
#! nix-shell -i "ghcid -T main"

import Control.Lens

splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn _ [] = []
splitOn f l@(x:xs)
    | f x = splitOn f xs
    | otherwise = let (h,t) = break f l in h:(splitOn f t)

{--
intCode :: Int -> Int -> Int -> [Int]
intCode [] = []
intCode (x:xs) = case x of 
                 0      -> [0]
                 1      -> [1]
                 99     -> []
                 _      -> intCode xs
--}

{--
intCode :: Int -> Int -> Int -> Int -> [Int] -> [Int]
intCode _ _ _ _ [] = []
IntCode a b c d (x:xs)
    | a == 1    = intCode replaceNth d ( b + c ) (x:xs)
    | a == 2    = intCode replaceNth d ( b * x ) (x:xs)
    | a == 99   = intCode a b c d []
    | otherwise = x:
--}
  
replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
    | n == 0 = newVal:xs
    | otherwise = x:replaceNth (n-1) newVal xs

indexOf' list element =
    let step l index = case l of
            [] -> Nothing
            (x:xs) ->
                if x == element
                    then Just index
                    else step xs (index + 1)
    in step list 0

group :: Int -> [a] -> [[a]]
group _ [] = []
group n l
    | n > 0 = (take n l) : (group n (drop n l))
    | otherwise = error "Negative or zero n"

main :: IO ()
main = do
    content <- readFile "02/02.txt"
    let opCodes  = splitOn (== ',') content
        newCodes = map (read::String->Int) opCodes
    let f x = do { print x >> print ( x!!0 )  >> print ( x!!1 ) >> print ( x!!2 ) >> print ( x!!3 ) } 
        in mapM_ f $ group 4 newCodes
    putStrLn "done"

{--
    let f list = do
        putStrLn $ "opCode is " ++ list!!1 
        putStrLn $ list!!2 ++ " " ++ (let a = list!!2 in list!!a)
        putStrLn $ list!!3 ++ " " ++ (let b = list!!3 in list!!b)
        putStrLn $ list!!4 ++ " " ++ (let c = list!!4 in list!!c)
        putStrLn $ "list: " ++ list
--}


