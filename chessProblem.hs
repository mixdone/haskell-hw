line = "_ _ _ _ _ _ _ _ "

prettyBoard :: (Int, Int) -> (Int, Int) -> String
prettyBoard (x1, y1) (x2, y2) = foldl(\acc x -> acc ++ (f x) ++ "\n") [] [1..8] where
  f str | str == x1 = foldl(\acc y -> if y == y1 then acc ++ "Q " else acc ++ "_ ") "" [1..8]
        | str == x2 = foldl(\acc y -> if y == y2 then acc ++ "q " else acc ++ "_ ") "" [1..8]
        | otherwise = line
{-
let ans = prettyBoard (1,2) (3,7)
putStrLn ans
        _ Q _ _ _ _ _ _
        _ _ _ _ _ _ _ _
        _ _ _ _ _ _ q _
        _ _ _ _ _ _ _ _
        _ _ _ _ _ _ _ _
        _ _ _ _ _ _ _ _
        _ _ _ _ _ _ _ _
        _ _ _ _ _ _ _ _

----------------------------------------------------------------------------------

let ans = prettyBoard (1,2) (3,7)
putStrLn ans
        _ Q _ _ _ _ _ _
        _ _ _ _ _ _ _ _
        _ _ _ q _ _ _ _
        _ _ _ _ _ _ _ _
        _ _ _ _ _ _ _ _
        _ _ _ _ _ _ _ _
        _ _ _ _ _ _ _ _
        _ _ _ _ _ _ _ _

-}
canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack (x1, y1) (x2, y2) | x1 == x2                        = True
                            | y1 == y2                        = True
                            | abs (x1 - x2) == abs (y1 - y2)  = True
                            | otherwise                       = False


{-
canAttack (1, 2) (3, 7)
False

canAttack (1, 2) (3, 4)
True

-}
