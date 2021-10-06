module StateTutorial where

import System.Random
import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State

data Die =
    DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show)

intToDie :: Int -> Die
intToDie 1 = DieOne
intToDie 2 = DieTwo
intToDie 3 = DieThree
intToDie 4 = DieFour
intToDie 5 = DieFive
intToDie 6 = DieSix
intToDie x = error $ "intToDie got non 1-6 integer: " ++ show x

rollDieThreeTimesNoState :: (Die, Die, Die)
rollDieThreeTimesNoState = do
    let s = mkStdGen 1
        (d1, s1) = randomR (1,6) s
        (d2, s2) = randomR (1,6) s1
        (d3, s3) = randomR (1,6) s2
    (intToDie d1, intToDie d2, intToDie d3)

rollDieWithState :: State StdGen Die
rollDieWithState = state $ do
    (n,s) <- randomR (1,6)
    return (intToDie n, s)