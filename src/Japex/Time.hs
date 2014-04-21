module Japex.Time
    (
        timesCommand
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
import System.Locale
import System.Random

timesCommand = Command times "times" "Generates times to spell" (usageInfo header options)

header = unlines [
    "Generates times (including ranges) to spell"
    , ""
    , "japex times"
    ]

options :: [OptDescr a]
options = []

data Mode = SingleTime | TimeRange
    deriving (Enum,Bounded,Show)

modeMin = minBound :: Mode
modeMax = maxBound :: Mode

instance Random TimeOfDay where
    random g = (timeToTimeOfDay . secondsToDiffTime $ v, ng)
        where (v,ng) = randomR (0, 24*60*60) g

inRandom f = liftM f get >>= \(a, g) -> put g >> return a

instance Random Mode where
    random = randomR (modeMin,modeMax)
    randomR (a,b) g = (toEnum v,ng)
        where (v,ng) = randomR (fromEnum a, fromEnum b) g

times _ = do
    mode <- randomIO :: IO Mode
    newStdGen >>= putStrLn . genData mode

genData :: RandomGen g => Mode -> g -> String
genData m r = case m of
    SingleTime -> format . head $ times
    TimeRange -> unwords . map format . sort . take 2 $ times
    where times = randoms r :: [TimeOfDay]
          format = formatTime defaultTimeLocale "%R"
