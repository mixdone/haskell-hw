myHead :: [Int] -> Int
myHead [] = error "empty"
myHead (x:_) = x

myTail :: [Int] -> [Int]
myTail [] = error "empty"
myTail (x:xs) = xs

myLast :: [Int] -> Int
myLast [] = error "empty"
myLast [x] = x
myLast (x:xs) = myLast xs

myInit :: [Int] -> [Int]
myInit [] = error "empty"
myInit [x] = []
myInit (x:xs) = x:(myInit xs)

myLength :: [Int] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

myNull :: [Int] -> Bool
myNull [] = True
myNull (_:_) = False

myDrop :: Int -> [Int] -> [Int]
myDrop 0 (x:xs) = (x:xs)
myDrop _ [] = []
myDrop n (x:xs) = myDrop (n - 1) xs

mySum :: [Int] -> Int
mySum [] = error "empry"
mySum [x] = x
mySum (x:xs) = x + mySum xs

myProduct :: [Int] -> Int
myProduct[] = error "empry"
myProduct [x] = x
myProduct (x:xs) = x * myProduct xs

myElem :: String -> String -> Bool
myElem _ [] = False
myElem n (x:xs) | x == n = True
                | x /= n = myElem n xs

myReverse :: [Int] -> [Int]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]
