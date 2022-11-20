--1
printf::String -> String -> String
printf str args = if (pr str)++(prf str args) == "" then str else (pr str)++(prf str args) where
  prf "" _ = ""
  prf ('%':'s': xs) args = args++xs
  prf (x:xs) args = prf xs args

  pr "" = ""
  pr str | ((last str) == 's') && ((last (init str)) == '%') = init (init str)
  pr str =  pr (init str)

--Math

--1
dectobin :: Int -> [Int]
dectobin num | num == 1 = [1]
             | num == 0 = [0]
dectobin num = (num `mod` 2) :(dectobin (num `div` 2))

first n = reverse $ dectobin n


--2
todec:: Int -> [Int] -> Int
todec base (x:xs) = foldl (\acc x -> (acc * base) + x) 1 xs

--3
strtoint :: String -> Int
strtoint n = read n :: Int


--4
findloss :: [Int] -> Int -> Int -> [Int]
findloss xs begin end = [x | x <- [begin..end], elem x xs == False]


--different

--1

star:: String -> Int -> Int -> Bool
star str left right | left < right = False
star "" left right | left == right = True
                   | otherwise = False
star str left right | (head str) == ')' = star (init str) left (right + 1)
                    | (head str) == '(' = star (init str) (left + 1) right

seq1 str = star str 0 0
