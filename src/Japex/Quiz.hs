module Japex.Quiz (
      quizCommand
    ) where

import Data.List
import Data.Time.Clock
import Data.Time.Format
import Japex.Common
    (
        Command(Command)
        , getJapexUserDataSubDir
    )
import System.Console.GetOpt
import System.Environment(getProgName)
import System.Exit
import System.FilePath
import System.IO
import System.Locale
import System.Random

data Options = Options {
                  lineCount :: Int
                , cats :: [String]
                , dbFile :: FilePath
                }

quizCommand = Command doQuiz printHelp
                
defaultOptions = Options 10 [] "/usr/share/japex/japex.db"

doQuiz args = do
    seed <- randomIO :: IO Int
    io args $ selectExs seed

selectExs randomSeed opts exs = randomize randomSeed (lineCount opts) . 
                                    filterByCats (cats opts) $ exs

io args f = do 
        ops <- processArgs args
        fileContents <- readFile . dbFile $ ops
        answerMap <- quiz . f ops . extractExs . lines $ fileContents
        resultFileName <- generateResultFileName
        withFile resultFileName WriteMode $ wAnswers answerMap

generateResultFileName = do
    quizSubDir <- getJapexUserDataSubDir "quiz"
    now <- getCurrentTime
    return $ joinPath [quizSubDir, formatTime defaultTimeLocale "%s" now]

argError m = printHelp >> (ioError . userError $ m)

printHelp = putStrLn . usageInfo header $ japexOpts
          where header = "Usage: japex quiz [OPTIONS]"
        
processArgs args
    | e /= [] = argError . init . concat $ e
    | length a > 1 = argError "Too many arguments for quiz"
    | otherwise = return o
    where (userOs, a, e) = getOpt Permute japexOpts args
          o = overrideDefault defaultOptions userOs
          overrideDefault = foldl (flip id) 
    
japexOpts = [
    Option "c" ["categories"]
        (ReqArg (\ cs opts -> opts { cats = splitCategories cs })
         "CATEGORIES")
            "categories from which to pick exercises"
    , Option "n" [""]
        (ReqArg (\ n opts -> opts { lineCount = read n })
         "NUM")
            "number of exercises"
    , Option "f" ["file"]
            (ReqArg (\ file opts -> opts { dbFile = file })
             "DB_FILE")
            "database file to use"
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
