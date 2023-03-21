import Control.Monad.State
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer.CPS
import qualified Data.Map as MP
import Data.Char

-- fix this!!!
dD :: Double -> Double -> Double -> Double
dD a b c = b^2 - 4 * a * c

solveQ :: Double -> Double -> Double -> Maybe (Double, Double)
solveQ a b c = do
  guard((dD a b c) >= 0)
  d <- (Just (dD a b c))
  return ((((-b) + (sqrt d)) / (2 * a)), (((-b) + (sqrt d)) / (2 * a)))

-- fix this code.
solveUserQ :: MaybeT IO (Double, Double)
solveUserQ = do
  input <- lift getLine -- (1,2,3)
  let (a, b, c) = read input :: (Double, Double, Double)
  r <- MaybeT . pure $ solveQ a b c
  return r




-------------------------------------------------------------------------------------------------




type Log = [String]
data UserInfo = UserInfo { address :: String, name :: String, salary :: Int } deriving Show


makeSureUserIsComfortableGivingInformation :: String -> MaybeT IO ()
makeSureUserIsComfortableGivingInformation infoName = do
  ans <- lift $ putStrLn infoName >> getLine
  guard((map toLower ans) == "yes")


getUserInfo :: WriterT Log (MaybeT IO) UserInfo
getUserInfo = do
  address <- getUserAddress
  name <- getUserName
  salary <- getUserSalary
  tell ["user's info"]
  return $ UserInfo address name salary

runGetUserInfo
getUserSalary :: WriterT Log (MaybeT IO) Int
getUserSalary = do
  lift $ makeSureUserIsComfortableGivingInformation "Do you comfortable to share your salary?"
  ans <- lift . lift $ putStrLn "Write your salary here" >> getLine
  let salaryAmt = read ans :: Int
  tell ["salary"]
  return salaryAmt

getUserName :: WriterT Log (MaybeT IO) String
getUserName = do
  lift $ makeSureUserIsComfortableGivingInformation "Do you comfortable to share your name"
  ans <- lift . lift $ putStrLn "Write your name here" >> getLine
  tell ["name"]
  return ans
-- same as getUserSalary

getUserAddress :: WriterT Log (MaybeT IO) String
getUserAddress = do
  lift $ makeSureUserIsComfortableGivingInformation "Do you comfortable to share your address"
  ans <- lift . lift $ putStrLn "Write your address here" >> getLine
  tell ["address"]
  return ans

runGetUserInfo :: IO ()
runGetUserInfo = do
    res <- runMaybeT . runWriterT $ getUserInfo
    print res
