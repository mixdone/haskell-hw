--13, 14
cubsumr :: [Int] -> Int
cubsumr = foldr (\x acc -> acc + x^3) 0
-- cubsumr [0,1,2,3]

cubsuml :: [Int] -> Int
cubsuml = foldl (\acc x -> acc + x^3) 0
-- cubsuml [0,1,2,3]

--15
fact :: Int -> Int
fact x = foldr (*) 1 [1..x]
expT :: Double -> Int -> Double
expT x n = foldl (\acc z -> acc + (x^z)/(fromIntegral (fact z))) 1 [1..n]
--expT 1 10

--16
howmany :: Eq a => a -> [a] -> Int
howmany x [] = error "empty list"
howmany n xs = foldl (\acc x -> if (x == n) then acc + 1 else acc) 0 xs
---- howmany 'a' "abcdaa

--17
howmany_g_b_letters :: [Char] -> (Int,Int)
howmany_g_b_letters xs = (good, bad) where
  good  = foldl (\acc x -> if elem x gs then acc + 1 else acc) 0 xs where
          gs = ['a', 'e', 'i', 'o', 'u']
  bad   = foldl (\acc x -> if elem x bs then acc + 1 else acc) 0 xs where
          bs = ['t', 'n', 'r', 's', 'h']
--owmany_g_b_letters "hello world"

--18
intersperse :: a -> [a] -> [a]
intersperse e xs = foldl (\acc x -> acc ++ [x] ++ [e]) [] xs
--intersperse ' ' "helloworld"

--19
cycleshift (x:xs) = xs ++ [x]

rotl n xs = foldl (\acc x -> cycleshift acc) xs [1..n]
rotatel xs = foldl (\acc n -> acc ++ [rotr n xs]) [xs] [1..(length xs - 1)]
--rotatel [1..7]

rotr n xs = foldr (\x acc -> cycleshift acc) xs [1..n]
rotater xs = foldr (\n acc -> acc ++ [rotr n xs]) [xs] [1..(length xs - 1)]
--rotater [1..7]
