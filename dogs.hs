
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
goodBoys :: [Dog]
goodBoys = [x | x <- dogs, isGoodBoy x == True]

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
