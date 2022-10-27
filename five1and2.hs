--1
quadraticSolver :: Double -> Double -> Double -> Maybe (Double, Double)
quadraticSolver a b c | d < 0     = Nothing
                      | otherwise = Just ((-b + sqrt d) / 2 / a, (-b - sqrt d) / 2 / a)
                          where d = b*b - 4*a*c

--2
maybeHead :: [a] -> Maybe a
maybeHead []     = Nothing
maybeHead (x:xs) = Just x

maybeTail :: [a] -> Maybe [a]
maybeTail []     = Nothing
maybeTail (x:xs) = Just xs

maybeInit :: [a] -> Maybe [a]
maybeInit []     = Nothing
maybeInit (x:xs) = foldr (f) Nothing (x:xs)
     where
       f _ Nothing   = Just []
       f x (Just xs) = Just (x:xs)

maybeFind :: (a -> Bool) -> [a] -> Maybe a
maybeFind predicate = fst . (foldl (\(a, b) x -> if predicate x && b == 0 then (Just x, 1) else (a, b)) (Nothing, 0))
