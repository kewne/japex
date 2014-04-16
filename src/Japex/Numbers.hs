module Japex.Numbers
    (
        numbersCommand
    ) where

import Japex.Common
import System.Console.GetOpt
import System.Random

numbersCommand = Command numbers (putStrLn $ usageInfo header options)

header = unlines [
    "Generates a random sequence of digits to spell"
    , ""
    , "japex numbers [OPTIONS]"
    ]

numbers args = do
    ran <- newStdGen
    either (ioError . userError) (putStrLn . concatMap show . numberFromOptions (digitSeq ran)) $ parseArgs args

numberFromOptions :: [Int] -> NumberOptions -> [Int]
numberFromOptions seq o = flip take seq $ numberLen o
    
digitSeq :: RandomGen g => g -> [Int]
digitSeq = randomRs (0, 9)

data NumberOptions = NumberOptions {
    numberLen :: Int
}

options = [
    Option "n" ["length"] (ReqArg (\ n o -> o { numberLen = read n }) "LENGTH") "The length of the sequence to generate (default: 10)"
    ]

defaultOptions = NumberOptions 10

parseArgs :: [String] -> Either String NumberOptions
parseArgs args = case getOpt Permute options args of
    (o, a, []) -> Right $ foldr id defaultOptions o
    (_, _, e) -> Left $ unlines e
