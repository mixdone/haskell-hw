--------------List comperhactoin--------------------------------
--1
fizzbuzz (x:xs) = [ if (x `mod` 15) == 0 then
    "fizzbuzz"
  else if (x `mod` 3) == 0 then
    "fizz"
  else if (x `mod` 5) == 0 then
    "buzz"
  else "" | x <- (x:xs)]
list = fizzbuzz [0..]
------------------------------------------------
--2
dotsInCircle :: (Double, Double) -> Double -> [(Double,Double)] -> [(Double, Double)]
dotsInCircle (_, _) 0 (_:_) = []
dotsInCircle (_, _) _ [] = []
dotsInCircle (x0, y0) r ((x,y):xs) = [if sqrt((x0 - x)^2 + (y0 - y)^2) < r  then (x, y) else (x0,y0) | (x, y) <- ((x, y):xs)]

--dotsInCircle (x0, y0) r ((x,y):xs) = [(x, y) | (x, y) <- ((x, y):xs), sqrt((x0 - x)^2 + (y0 - y)^2) < r]


----------------------------------------------
--3
setand::[Int] -> [Int] -> [Int]
setand xs1 (x:xs) = [x | x <- (x:xs), elem x xs1]


------------ Math -------------------

--1
count:: Int -> Int -> Int
count 0 n = n
count x n = count (x `div` 10) (n + 1)

amount x = count x 0
---------------------------------------------------------
--2
sumdigits::Int -> Int -> Int
sumdigits n 0 = n
sumdigits n x = sumdigits (n + x `mod` 10) (x `div` 10)

sum1 x = sumdigits 0 x

---------------------------------------------------------
--3
power2::Int -> Bool
power2 0 = True
power2 2 = True
power2 x | x `mod` 2 == 0 = power2 $ x `div` 2
        | otherwise = False
---------------------------------------------------------
--4

seqbyPred::Int -> Int -> Int -> [Int]
seqbyPred _ _ 0 = []
seqbyPred x n l = [x] ++ seqbyPred (x + n) n (l-1)


sequensebyPred::Int -> Int -> [Int]
sequensebyPred n k = n: sequensebyPred (n + k) k
listseq k = sequensebyPred 1 k

res = listseq $ (\x -> x*3) 2

---------------------------------------------------------
--5
fibonacci::Int -> Int -> [Int]
fibonacci n k = n:fibonacci k (n+k)
fib = fibonacci 0 1
---------------------------------------------------------
--6
collatz::Int -> Int -> Int -> (Int, Int)
collatz m n 1 = (m, n)
collatz m n x | x `mod` 2 == 0 = collatz (max m x)(n + 1) (x `div` 2)
              | otherwise = collatz m (n + 1) (3*x + 1)

coll x = collatz 0 0 x

---------------------------------------------------------
--7
logarifm :: Int -> Int -> Int
logarifm x k | 2^k >= x = k
             | otherwise = logarifm x (k + 1)

lg x = logarifm x 0


---------------------------------------------------------
--8

quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (x:xs) = less ++ [x] ++ bigger
    where less = quicksort [n | n <- xs, n <= x]
          bigger = quicksort [k | k <- xs, k > x]
