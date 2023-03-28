import Data.List
data DogBreed = GoldenRetrievers
              | BostonTerriers
              | LabradorRetrievers
              | Poodles
              | BorderCollie
              | Beagle
              | IrishSetter
              | Staffordshire
              | Bull
              | Terrier
    deriving (Show, Eq)

data Gender = Male | Female
    deriving (Show, Eq)

data Dog = Dog { name :: String
               , age :: Int
               , gender :: Gender
               , breed :: DogBreed
               , isGoodBoy :: Bool -- holds for female dogs as well
               } deriving (Show, Eq)

dogs = [ Dog "Leander" 12 Male Beagle False
       , Dog "Ouranos" 1 Male Poodles True
       , Dog "Pegasus" 2 Female Beagle False
       , Dog "Atlas" 8 Female GoldenRetrievers True
       , Dog "Castor" 6 Male LabradorRetrievers True
       , Dog "Apollo" 3 Female Beagle False
       , Dog "Narkissos" 15 Male Beagle True
       , Dog "Dardanos" 7 Female Terrier True
       , Dog "Ajax" 4 Male IrishSetter False
       , Dog "Pyrrhos" 2 Female BorderCollie False
       , Dog "Patroclus" 6 Male Bull True
       , Dog "Iacchus" 4 Female Beagle True ]

-- dogs which are good boys
goodBoys :: [Dog] -> [Dog]
goodBoys list = [x | x <- list, isGoodBoy x == True]

-- dogs with name longer than 7 symbols
longNamedDogs :: [Dog]
longNamedDogs = [x | x <- dogs, (length $ name x) > 7]

-- among dogs, which is the most popular gender?
mostPopularDogGender :: Gender
mostPopularDogGender = if male > female then Male else Female where
  male    = foldr (\x male   -> if (gender x) == Male   then male + 1   else male)   0 dogs
  female  = foldr (\x female -> if (gender x) == Female then female + 1 else female) 0 dogs


oldestDog :: Dog
oldestDog = foldr (\x acc -> if (age x) > (age acc) then x else acc) (head dogs) dogs

averageDogAge :: Double
averageDogAge = (fromIntegral sumage) / (fromIntegral $ length dogs) where
  sumage = foldr (\x acc -> acc + (age x)) 0 dogs

-- finds dogs with given breed
dogsByBreed :: DogBreed -> [Dog]
dogsByBreed givenBreed = [x | x <- dogs, (breed x) == givenBreed]
--dogsByBreed Beagle


dogsAge :: Int -> [Dog]
dogsAge n = filter (\x -> (age x) == n) dogs

dogsAge246 :: [(Dog, Dog, Dog)]
dogsAge246 = (dogsAge 2) >>= (\x ->
                (dogsAge 4) >>= (\y ->
                  (dogsAge 6) >>= (\z ->
                    return $ (x,y,z)
                    )
                  )
                )

isMaleOrFemale :: [Dog] -> Gender -> [Dog]
isMaleOrFemale list currentGender = filter(\x -> gender x == currentGender) list

notIrishSetter :: [Dog] -> [Dog]
notIrishSetter list = filter (\x -> breed x /= IrishSetter) list

nameLongerThan :: [Dog] -> Int -> [Dog]
nameLongerThan list n = filter(\x -> (length $ name x) > n) list


{-
makeListOfTuples :: Dog -> [Dog] -> [(Dog , Dog)]
makeListOfTuples _ [] = []
makeListOfTuples maleDog femaleDogs = [( maleDog , (head femaleDogs) )]
                                      ++ makeListOfTuples maleDog (tail femaleDogs) -}

maleDogs :: [Dog]
maleDogs = do
  firstDog <- (dogsAge 4) ++ (dogsAge 5)
  firstDog <- goodBoys        (return firstDog)
  firstDog <- isMaleOrFemale  (return firstDog) Male
  firstDog <- notIrishSetter  (return firstDog)
  return firstDog

femaleDogs :: [Dog]
femaleDogs = do
  secondDog <- (dogsAge 4) ++ (dogsAge 5)
  secondDog <- isMaleOrFemale (return secondDog) Female
  secondDog <- nameLongerThan (return secondDog) 4
  return secondDog

dogsQuery :: [[(Dog, Dog)]]
dogsQuery = do
  male   <- permutations maleDogs
  female <- permutations femaleDogs
  return $ zip male female
  
   
