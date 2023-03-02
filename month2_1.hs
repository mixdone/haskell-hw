import Prelude hiding ((!!),init,reverse,(++),cycle,take,elem)
--1
(!!)::[a] -> Int -> a
(!!) xs n | n < 0 = error "Element should be positive!"
(!!) [] _         = error "Element out of list!"
(!!) xs 0         = head xs
(!!) xs n         = (!!) (tail xs) (n-1)
--['a'..'z'] !! 5

--2
init :: [a] -> [a]
init []           = error "Empty list!"
init [x]          = []
init (x:xs)       = x:(init xs)
--init [0..6]

--3
(++) :: [a] -> [a] -> [a]
(++) [] ys        = ys
(++) (x:xs) ys    = x : (++) xs ys
-- [1..5] ++ [6..9]

--4
cycle :: [a] -> [a]
cycle xs = (++) xs (cycle xs)
-- take 25  cycle [1..5]

--5
take:: Int -> [a] -> [a]
take n _ | n < 0  = error "number should be positive"
take n []         = []
take n xs         = take' n xs where
    take' n []      = error "not enough elements in the list"
    take' 0 xs      = []
    take' n (x:xs)  = x : (take' (n - 1) xs)
--take 50 [1..]

--6
inits:: [a] -> [[a]]
inits []        = error "Empty list"
inits [x]       = [[], [x]]
inits xs        = (++) (inits $ init xs) [xs]
--inits ['a'.. 'h']

tails:: [a] -> [[a]]
tails []        = error "Empty list"
tails [x]       = [[x], []]
tails xs        = (++) [xs] (tails $ tail xs)
--tails [1..7]

--7
elem :: Eq a => a -> [a] -> Bool
elem _ []                     = False
elem n (x:xs) | n == x        = True
              | otherwise     = elem n xs
--elem 'd' ['1'..'9']
--elem 'd' ['a'..'f']

--8
nub:: Eq a => [a] -> [a]
nub []                           = []
nub (x:xs) | elem x xs == True   = nub xs
           | elem x xs == False  = (++) [x] (nub xs)
--nub "HHhii, WWoooorllldd!"

--9
updElmBy :: [a] -> Int -> a -> [a]
updElmBy [] _ _         = error "empty list"
updElmBy _ i _ | i < 0  = error "index should be positive"
updElmBy xs i x         = (++) first_part second_part where
  first_part    = (++) (take i xs) [x]
  second_part   = drop (i + 1) xs
--updElmBy ['a'..'z'] 7 '#'

--10
swp :: [a] -> Int -> Int -> [a]
swp [] _ _                   = error "empty list"
swp xs i j | i < 0 || j < 0  = error "index should be positive"
           | i > j           = swp xs j i
swp xs i j                   = first ++ second ++ taIl where
  first     = (take i xs) ++ [xs!!j]
  second    = (take (j - i - 1) (drop (i + 1) xs)) ++ [xs!!i]
  taIl      = drop (j + 1) xs
--swp [0..20] 6 15

--11
permutations :: Eq a => [a] -> [[a]]
permutations [] = [[]]
permutations xs = [first_el:tail_permutations |
 first_el <- xs, tail_permutations <- permutations (remove first_el xs)] where
   remove el (x:xs) | el == x   = xs
                    | otherwise       = [x] ++ (remove el xs)
--permutations [1..3]
--permutations [1,2]

--12
subsequences :: Eq a => [a] -> [[a]]
subsequences xs = helpfunction (length xs) xs where
  helpfunction 0 _ = [[]]
  helpfunction depth xs = (makesequence depth xs) ++ (helpfunction (depth - 1) xs) where
    makesequence 0 xs = [[]]
    makesequence len xs = [(xs!!i):ys | i <- [0..(length xs - 1)], ys <- makesequence (len - 1) (remove (i+1) xs)] where
      remove 0 xs = xs
      remove i xs = remove (i - 1) (tail xs)
--subsequences [1,2,2,3]
--subsequences [1,2,3,4,5]
