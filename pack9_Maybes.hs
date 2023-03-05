import Prelude hiding (head, tail, maximum)

type GreekData = [(String, [Integer])]

greekDataA :: GreekData
greekDataA = [ ("alpha", [5, 10])
             , ("beta", [0, 8])
             , ("gamma", [18, 47, 60])
             , ("delta", [42])
             ]

greekDataB :: GreekData
greekDataB = [ ("phi", [53, 13])
             , ("chi", [21, 8, 191])
             , ("psi", [])
             , ("omega", [6, 82, 144])
             ]

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x:xs) = Just x

divMay :: Double -> Double -> Maybe Double
divMay _ 0 = Nothing
divMay x y = Just $ x / y

maximumMay :: Ord a => [a] -> Maybe a
maximumMay [] = Nothing
maximumMay (x:xs) = Just $ foldr(\x acc -> if x > acc then x else acc) x xs

tailMay :: Ord a => [a] -> Maybe a
tailMay [] = Nothing
tailMay (x:xs) = maximumMay xs

undoInt :: Maybe Integer -> Integer
undoInt Nothing  = 0
undoInt (Just x) = x

undolist :: Maybe [a] -> [a]
undolist Nothing  = []
undolist (Just xs) = xs

{-
 tl;dr implement the function WITHOUT do-notation or any monad magic. only pattern-matching, where and let in

 first query the GreekData that is passed in,
 look up the string passed in the second argument,
 and retrieve the corresponding list of Integers. Call this list xs.
 Next calculate the mawximum of the tail of xs
 (Don’t use any pattern matching here.
 Use case expressions and the maximumMay and tailMay functions)
 Take the maximum and divide it by the head of the list (using headMay and divMay functions).
 If any of these operations along the way return Nothing, then your function should return Nothing.
 But if everything succeeds, then return the final quotient.
 One hint… you’ll need to use the fromIntegral function to convert your two Integers to Doubles for the final call to divMay.
-}


intermediate :: [Integer] -> Maybe Double
intermediate xs = divMay (fromIntegral $ undoInt $ tailMay xs) (fromIntegral $ undoInt $ headMay xs)


queryGreek :: GreekData -> String -> Maybe Double
queryGreek gData str | gData == greekDataA = intermediate $ undolist $ lookup str gData
                     | gData == greekDataB = intermediate $ undolist $ lookup str gData
                     | otherwise           = Nothing



-- Предполлагаю, что этот case должен выгледеть так, но case всегда выбирает greekDataA... С if работает нормально ниже

-- queryGreek greekDataA "alpha" == Just 2.0


-- Now do the same whole thing, but using do-notation, since Maybe is a Monad
queryGreekPro :: GreekData -> String -> Maybe Double
queryGreekPro gData str = do
  xs          <- if gData == greekDataA then lookup str gData else lookup str gData
  first_arg   <- tailMay xs
  second_arg  <- headMay xs
  ans         <- divMay (fromIntegral first_arg) (fromIntegral second_arg)
  return ans

-- * a harder task. rewrite queryGreekPro, but without the do-notation, only using the (>>=) operator and its friends
-- in other words, desugarize your notation
queryGreekProPlus :: GreekData -> String -> Maybe Double
queryGreekProPlus gData str = (lookup str gData) >>= (fromIntegral $ undoInt tailMay) >>= divMay ((lookup str gData) >>= (fromIntegral $ undoInt headMay))


-- Доделать 
