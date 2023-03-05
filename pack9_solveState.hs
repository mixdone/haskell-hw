import Control.Monad.State

-- example:
{-
solveState "x=foo y=bar y=$x l=l x=$y"
-}
{-
At the end of this program the state is:

[("x","foo"),("y","foo"),("l","l")]

-}

type VarState = [(String, String)]

isnot :: String -> [(String, [a0])] -> Bool
isnot _ [] = True
isnot y ((x, _):xs) | y == x    =  False
                    | otherwise = isnot y xs

first_arg :: [Char] -> [Char]
first_arg xs = fst $ foldl(\(accS, accBool) x -> if accBool == 0 then if (x == '=') then (accS, 1) else (accS ++ [x], accBool) else (accS, accBool)) ([], 0) xs

second_arg :: [Char] -> [Char]
second_arg xs = fst $ foldl(\(accS, accBool) x -> if accBool == 0 then (if (x == '=') then (accS, 1) else (accS, accBool)) else (accS ++ [x], accBool)) ([], 0) xs

getVars :: [String] -> [(String, [a0])]
getVars lst = foldl(\acc cur -> if (isnot (first_arg cur) acc) then acc ++ [(first_arg cur, [])] else acc) [] lst

link :: VarState -> String -> String
link ((var, val): xs) lnk | lnk == var = val
                          | otherwise  = link xs lnk


changeState :: String -> VarState -> VarState -> VarState
changeState str ((var, val):xs) st | first_arg str /= var = ((var, val):(changeState str xs st))
                                   | otherwise = ((var, value):xs) where
                                      value = if (head (arg) == '$') then link st (tail arg) else arg
                                      arg = second_arg str


func :: [String] -> State VarState [(String, String)]
func list = do                              {- list of words like "_=_"-}
  put $ getVars list
  currentState <- get
  put $ foldl (\acc x -> changeState x acc acc) currentState list
  currentState <- get
  return currentState


solveState :: String -> [(String, String)]
solveState str = evalState (func $ words str) [([], [])]
