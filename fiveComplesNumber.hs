
data Comxplexnum = Comxplexnum { realpart :: Double
                               , ipart :: Double
                               } deriving (Show, Eq)

complexSum :: Comxplexnum -> Comxplexnum -> Comxplexnum
complexSum a1 a2 = Comxplexnum ((realpart a1) + (realpart a2)) ((ipart a1) + (ipart a2))

complexSub :: Comxplexnum -> Comxplexnum -> Comxplexnum
complexSub a1 a2 = Comxplexnum ((realpart a1) - (realpart a2)) ((ipart a1) - (ipart a2))

complexMult :: Comxplexnum -> Comxplexnum -> Comxplexnum
complexMult a1 a2 = Comxplexnum (real_real + i_i) (real_i + i_real) where
  real_real = (realpart a1) * (realpart a2)
  real_i = (realpart a1) * (ipart a2)
  i_real = (realpart a2) * (ipart a1)
  i_i = (ipart a2) * (ipart a1)

complexDiv :: Comxplexnum -> Comxplexnum -> Comxplexnum
complexDiv a1 a2 = Comxplexnum (real) (i) where
  real = ((realpart a1) * (realpart a2) + (ipart a1) * (ipart a2)) / ((realpart a2)^2 + (ipart a2)^2)
  i = ((realpart a2) * (ipart a1) - (realpart a1) * (ipart a2)) / ((realpart a2)^2 + (ipart a2)^2)
