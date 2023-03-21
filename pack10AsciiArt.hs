import Data.Char

num :: [Int]
num = [0, 0] ++ [x | x <- [1..], x `mod` 2 /= 0]

stars :: Int -> String
stars 0 = ""
stars n = "*" ++ (stars $ n-1)

makeString :: Int -> Int -> String
makeString 0 _ = ""
makeString 1 k = (stars (((num !! k) + 1) `div` 2)) ++ "a" ++ (stars(((num !! k) + 1) `div` 2)) ++ "\n"
makeString n k = (stars ((k - (num !! n) + 2) `div` 2)) ++ [chr (n + 96)] ++ (stars (num !! n)) ++ [chr (n + 96)] ++ (stars ((k - (num !! n) + 2) `div` 2)) ++ "\n"


makeArt :: Int -> String
makeArt n = inc ++ dec where
      inc = foldl(\acc x -> acc ++ (makeString x n)) "" [1..n-1] 
      dec = foldr(\x acc -> acc ++ (makeString x n)) "" [1..n]
