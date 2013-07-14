module Japex.Quiz (
      doQuiz
    ) where

import Data.List
import Japex.Common
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import System.Random

data Options = Options {
                  lineCount :: Int
                , cats :: [String]
                , help :: Bool
                }
                
defaultOptions = Options 10 [] False

doQuiz args = do
    seed <- randomIO :: IO Int
    io args $ selectExs seed

io args f = do 
        (ops, dbFile) <- processArgs args
        fileContents <- readFile dbFile
        answerMap <- quiz . f ops . extractExs . lines $ fileContents
        withFile "results.txt" WriteMode $ wAnswers answerMap

selectExs randomSeed opts exs = randomize randomSeed (lineCount opts) . 
                                    filterByCats (cats opts) $ exs

argError m = printHelp >> (ioError . userError $ m)

printHelp = putStrLn . usageInfo header $ japexOpts
          where header = "Usage: japex quiz [OPTIONS] DB_FILE"
        
processArgs args
    | e /= [] = argError . init . concat $ e
    | help o = printHelp >> exitSuccess
    | length a < 1 = argError "No database file specified"
    | length a > 1 = argError "Too many files specified"
    | otherwise = return (o, head a)
    where (userOs, a, e) = getOpt Permute japexOpts args
          o = overrideDefault defaultOptions userOs
          overrideDefault = foldl (flip id) 
    
japexOpts = [
                Option "c" ["categories"]
                    (ReqArg (\ cs opts -> opts { cats = splitCategories cs })
                        "CATEGORIES")
                    "categories from which to pick exercises"
            ,   Option "n" [""]
                    (ReqArg (\ n opts -> opts { lineCount = read n })
                        "NUM")
                    "number of exercises"
            ,   Option "h" ["help"] 
                    (NoArg (\ opts -> opts { help = True }))
                     "print this help message"
            ]

randomize seed size exs = map (exs !!) randomIndexes
    where randomIndexes = take size . randomRs (0, numExs -1) $ mkStdGen seed 
          numExs = length exs

filterByCats cats = filter (`hasCats` cats)
    where hasCats (_,_,exCats) cats = null (cats \\ exCats)

extractExs = map splitFields

splitFields l = (jap,eng,cats)
    where (jap, engAndCats) = breakField l
          (eng, unsplitCats) = breakField . tail $ engAndCats
          cats = splitCategories . tail $ unsplitCats
          breakField = break (== ':')

splitCategories cs 
    | null rest = [cat]
    | otherwise = cat : splitCategories (tail rest)
    where (cat, rest) = break (== ',') cs

isInCategories (_,_,exCats) cats = (exCats `intersect` cats) == cats

quiz :: [(String, String, a)] -> IO [(String,String, String)]
quiz = mapM question
    where question (q,a,_) = do putStrLn q 
                                ua <- getLine
                                return (q,a,ua)
wAnswers answers h  = mapM_ (wSingleAnswer h) answers
    where wSingleAnswer h (q,a,ua) = hPutStrLn h $ intercalate ":" [q,a,ua]
