module Barans (
Sheep,
names,
father,
mother
) where


import Control.Monad
import Data.List

data Tree a = Leaf a | Branch (Tree a) a (Tree a)
type TreeList a = [Tree a]

fringe (Leaf x) = [x]
fringe (Branch left _ right) = fringe left ++ fringe right

kolvo :: Tree a -> Int
kolvo (Leaf _ ) = 1
kolvo (Branch l _ r) = kolvo l + kolvo r + 1

leftA :: Tree a -> Maybe (Tree a)
leftA (Leaf _)       = Nothing
leftA (Branch l _ r) = Just l

rightA :: Tree a -> Maybe (Tree a)
rightA (Leaf _)       = Nothing
rightA (Branch l _ r) = Just r

content :: Tree a -> a
content (Leaf x)       = x
content (Branch _ x _) = x

type Sheep = String

mother' :: Sheep -> Tree Sheep -> Maybe Sheep
mother' _ (Leaf _) = Nothing
mother' s (Branch l c r) = if (s == c) 
                            then Just (content l)  
                            else if (mother' s l) == Nothing then mother' s r else mother' s l

mother'' :: Sheep -> TreeList Sheep -> Maybe Sheep 
mother'' _ [] = Nothing
mother'' s (x:xs) = (mother' s x) `mplus` mother'' s xs

mother s = mother'' s [i10, i12]

father' :: Sheep -> Tree Sheep -> Maybe Sheep
father' _ (Leaf _) = Nothing
father' s (Branch l c r) = if (s == c) 
                            then Just (content r)  
                            else (father' s l) `mplus` (father' s r)

father'' :: Sheep -> TreeList Sheep -> Maybe Sheep 
father'' _ [] = Nothing
father'' s (x:xs) = (father' s x) `mplus` (father'' s xs)

father s = father'' s [i10, i12]

names' :: Tree Sheep -> [Sheep]
names' (Leaf x)       = [x]
names' (Branch l x r) = (names' l) ++ [x] ++ (names' r)

names'' :: TreeList Sheep -> [Sheep]
names'' [] = []
names'' (x:xs) = (names' x) `mplus` (names'' xs)

names = (sort . nub . names'') [i10, i12]

i8  = Branch (Branch (Leaf "i1") "i3" (Leaf "i2")) "i8" (Leaf "i7")
i9  = Branch (Leaf "i3") "i9" (Leaf "i5")
i10 = Branch i8 "i10" i9
i11 = Branch i8 "i11" i9
i6  = Branch (Leaf "i4") "i6" (Leaf "i5")
i12 = Branch i11 "i12" i6

{-
                      i12
          i10, i11
      i8           i9         i6
   i3    i7     i3   i5    i4    i5
i1    i2

-}





findMothersDad :: Sheep -> Maybe Sheep
findMothersDad s = do
  mom <- mother s
  dad <- father mom
  return dad

greatGrandFather :: Sheep -> Maybe Sheep
greatGrandFather s = do
  grandFather <- findMothersDad s
  great_      <- father grandFather
  return great_

runMaybeList :: [Maybe Sheep] -> [Sheep]
runMaybeList xs = foldl(\acc x -> if x /= Nothing then acc ++ [f x] else acc) [] xs where
  f :: Maybe a -> a
  f (Just n) = n 

findP :: Maybe Sheep -> [Maybe Sheep]
findP Nothing  = []
findP (Just s) = [mother s] ++ [father s] ++ (findP (father s)) ++ (findP (mother s)) 

findAllParents x = nub $ runMaybeList $ findP (Just x)
-- ищет всех предков

findP' :: Maybe Sheep -> [Maybe Sheep]
findP' Nothing  = []
findP' (Just s) = [mother s] ++ [father s]


findParents x = nub $ runMaybeList $ findP' (Just x)
-- ищет родителей

findGrandParents :: Sheep -> [Sheep]
findGrandParents x = nub $ runMaybeList $ moms ++ dads where
  moms = findP' (mother x) 
  dads = findP' (father x)
 


isOrphan :: Sheep -> Bool
isOrphan s | (mother s) /= Nothing = False
           | (father s) /= Nothing = False
           | otherwise             = True




selected_barans = ["i3", "i5", "i6", "i9", "i12"]

selectedDad :: Sheep -> Maybe Sheep
selectedDad s = do
  dad <- father s
  guard((elem dad selected_barans) == True)
  return dad


nearestSelected :: Sheep -> Maybe Sheep
nearestSelected s | father s      == Nothing = Nothing
                  | selectedDad s == Nothing = (father s) >>= nearestSelected
                  | otherwise                = selectedDad s


