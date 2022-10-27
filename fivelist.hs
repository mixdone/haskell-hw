data MyList a = EmptyList | MyList a (MyList a)
  deriving (Show, Eq)

instance Foldable MyList where
  foldr _ base EmptyList     = base
  foldr f base (MyList x xs) = f x $ foldr f base xs

fromList :: [a] -> MyList a
fromList = foldl (\acc x -> MyList x acc) EmptyList
--fromList [1,2,3,4]

toList :: MyList a -> [a]
toList (MyList x xs) = foldl (\acc x -> acc ++ [x]) [] (MyList x xs)
--toList (MyList 2 (MyList 3 (MyList 4 EmptyList)))

reverseMyList :: MyList a -> MyList a
reverseMyList (MyList x xs) = foldl (\acc x -> (MyList x acc)) EmptyList (MyList x xs)
--reverseMyList (MyList 2 (MyList 3 (MyList 4 EmptyList)))

mapMyList :: (a -> b) -> MyList a -> MyList b
mapMyList f (MyList x xs) = foldr (\x acc -> MyList (f x) acc) EmptyList (MyList x xs)
--mapMyList (\x -> x*2) (MyList 2 (MyList 3 (MyList 4 EmptyList)))
