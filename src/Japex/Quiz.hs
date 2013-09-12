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

quizCommand = Command doQuiz (putStrLn helpMessage)
                
doQuiz args = do
    seed <- randomIO :: IO Int
    quizExercises args $ selectExs seed

selectExs randomSeed opts = randomize randomSeed (lineCount opts) . 
                                    filterByCats (cats opts)

randomize seed size exs = map (exs !!) randomIndexes
    where randomIndexes = take size . randomRs (0, numExs -1) $ mkStdGen seed 
          numExs = length exs

filterByCats cats = filter (null . (cats \\ ) . \ (_,_,c) -> c)

quizExercises args f = do 
    ops <- processArgs args
    fileContents <- readFile . dbFile $ ops
    answerMap <- quiz . f ops . extractExs . lines $ fileContents
    resultFileName <- generateResultFileName
    writeFile resultFileName (toAnswerString answerMap)
        
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

argError m = putStrLn helpMessage >> (ioError . userError $ m)

helpMessage = usageInfo "Usage: japex quiz [OPTIONS]" japexOpts

defaultOptions = Options 10 [] "/usr/share/japex/japex.db"

data Options = Options {
                  lineCount :: Int
                , cats :: [String]
                , dbFile :: FilePath
                }

generateResultFileName = do
    quizSubDir <- getUserQuizDir
    now <- getCurrentTime
    return $ joinPath [quizSubDir, formatTime defaultTimeLocale "%s" now]

getUserQuizDir = getJapexUserDataSubDir "quiz"

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

quiz = mapM (\ (q,a,cats) -> do
                    putStrLn q 
                    ua <- getLine
                    return (q,a,ua)
            )

toAnswerString = unlines . map (\ (q,a,ua) -> intercalate ":" [q,a,ua])
