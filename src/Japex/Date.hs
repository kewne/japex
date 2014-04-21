module Japex.Date
    (
        datesCommand
    ) where

import Control.Applicative
import Control.Monad.State
import Data.List
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime
import Japex.Common
import System.Console.GetOpt
import System.Random

datesCommand = Command dates "dates" "Generates dates to spell" (usageInfo header options)

header = unlines [
    "Generates dates (including ranges) to spell"
    , ""
    , "japex dates"
    ]

options :: [OptDescr a]
options = []

data Mode = SingleDate | DateRange
    deriving (Enum,Bounded,Show)

modeMin = minBound :: Mode
modeMax = maxBound :: Mode

instance Random Day where
    random = randomR (ceilDay,topDay)
        where ceilDay = fromGregorian 1800 1 1
              topDay = fromGregorian 2500 12 31
    randomR (a,b) g = (addDays d a, ng)
        where (d,ng) = randomR (0, diffDays b a) g

inRandom f = liftM f get >>= \(a, g) -> put g >> return a

instance Random Mode where
    random = randomR (modeMin,modeMax)
    randomR (a,b) g = (toEnum v,ng)
        where (v,ng) = randomR (fromEnum a, fromEnum b) g

dates _ = do
    mode <- randomIO :: IO Mode
    newStdGen >>= putStrLn . genData mode

genData :: RandomGen g => Mode -> g -> String
genData m r = case m of
    SingleDate -> show . head . take 1 $ days
    DateRange -> unwords . map show . sort . take 2 $ days
    where days = randoms r :: [Day]

range = (-lim, lim)
    where lim = 365 * 500
