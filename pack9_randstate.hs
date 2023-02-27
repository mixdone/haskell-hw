import Control.Monad.State

type RandState = Int
-- state monad
rollDice :: State RandState Int
rollDice = state $ \seed -> ((if ((seed + 1) * seed) `mod` 6 == 0 then
                              ((seed + 1) * seed) `mod` 6 + 1
                            else ((seed + 1) * seed) `mod` 6),
                            (if ((seed + 1) * 7) `mod` 6 == 0 then
                              ((seed + 1) * 7) `mod` 6 + 1
                            else ((seed + 1) * 7) `mod` 6))
                            {- генератор рандомных чисел (пусть и псевдо)
                            должен казаться магией, так что не стоит это читать) -}

game :: State RandState String
game = do
    firstPlayerRes    <- rollDice
    secondPlayerRes   <- rollDice
    firstPlayerRes    <- rollDice
    if
      firstPlayerRes >= secondPlayerRes
    then
      return "First wins"
    else
      return "Second wins"

runGame :: String
runGame = evalState game startSeed
    where startSeed = 3
