import Control.Monad.State

-- see this program as example:
{-
x=foo
y=bar
y=$x
l=l
x=$y
-}
{-
At the end of this program the state is:
x = foo
y = foo
l = l
-}

exampleProgram :: String
exampleProgram = "x=foo\ny=bar\ny=$x\nl=l\nx=$y"

-- one of possible answers. order in list doesn't matter
exampleAns :: [(String, String)]
exampleAns = [("x", "foo"), ("y", "foo"), ("l", "l")]

check :: IO ()
check = do
    let resultState = solveState exampleProgram
    if exampleAns `listEq` resultState
        then putStrLn "OK!"
        else error "something wrong:("
    where listEq l r = leftInRight && rightInLeft
            where leftInRight = all (\x -> x `elem` r) l
                  rightInLeft = all (\x -> x `elem` l) r

data Command = Command { varName :: String, whatToPut :: String } deriving Show

-- you can choose something else!
type InterpreterState = [(String, String)]

solveState :: String -> [(String, String)]
solveState input = interpretToState (map parse $ words input)

-- example: "foo=bar" -> Command "foo" (Literal "bar")
-- example: "foo=$bar" -> Command "foo" (VariableReference "bar")

-- Delete Data Type Value, because I had issues with types matching
parse :: String -> Command
parse xs = Command name val where
  name = fst $ foldl(\(accS, accBool) x ->
          if    accBool == 0 then
          if    (x == '=') then
                (accS, 1)
          else  (accS ++ [x], accBool)
          else  (accS, accBool))
                ([], 0) xs
  val  = fst $ foldl(\(accS, accBool) x ->
          if    accBool == 0 then
          if    (x == '=') then
                (accS, 1)
          else  (accS, accBool)
          else  (accS ++ [x], accBool))
                ([], 0) xs

-- you may rewrite this. e.g. you can use fold
-- but if you look at standard library there might be
-- a better alternative for chaining state functions.
-- In other words, executing a list of (State s a)
-- functions is a common task, and it has a standard implementation
getvalfromlink :: String -> InterpreterState -> String
getvalfromlink x [] = error "something wrong("
getvalfromlink x ((var,val):xs) | x == var  = val
                                | otherwise = getvalfromlink x xs

interpretMany :: [Command] -> State InterpreterState ()
interpretMany [] = return ()
interpretMany (x:xs) = do
  interpretOne x
  interpretMany xs

-- using get, set and other State functions, interpret the command
interpretOne :: Command -> State InterpreterState ()
interpretOne command = do
  currentState <- get
  let node | elem '$' (whatToPut command) = [(varName command, getvalfromlink (tail $ whatToPut command) currentState)]
           | otherwise = [(varName command, whatToPut command)]

  put $ [x | x <- currentState, fst x /= varName command] ++ node
  return ()

-- you can choose other type for result
interpretToState :: [Command] -> [(String, String)]
interpretToState commands = tail $ execState (interpretMany commands) emptyState where
  emptyState = [([],[])]
