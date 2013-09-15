module Japex.Quiz (
      quizCommand
    ) where

import Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as TIo
import Japex.Common
    (
        Command(..)
        , getJapexUserDataSubDir
    )
import Japex.Quiz.Common
import Japex.Quiz.File
import System.Console.GetOpt
import System.Environment(getProgName)
import System.Exit
import System.IO
import System.Random

quizCommand = Command doQuiz (putStrLn helpMessage)

doQuiz :: [String] -> IO ()
doQuiz args = do
    ops <- processArgs args =<< randomIO
    questions <- readQuizDatabaseFile . dbFile $ ops
    answers <- quiz . selectExs ops $ questions
    writeAnswerFile answers

selectExs :: Options -> [QuizEntry] -> [QuizEntry]
selectExs (Options lineCount cats _ seed) = take lineCount . shuffle seed . filterByCats cats

shuffle :: Int -> [QuizEntry] -> [QuizEntry]
shuffle seed exs = map (exs !!) $ randomRs range $ mkStdGen seed
    where range = (0, length exs -1)

filterByCats :: [T.Text] -> [QuizEntry] -> [QuizEntry]
filterByCats cats = filter (null . (cats \\ ) . categories)
        
processArgs args seed
    | not . null $ e = argError . init . concat $ e
    | not . null $ a = argError "Too many arguments for quiz"
    | otherwise = return o
    where (userOs, a, e) = getOpt Permute japexOpts args
          o = overrideDefault (defaultOptions seed) userOs
          overrideDefault = foldl (flip id) 

japexOpts = [
    Option "c" ["categories"]
        (ReqArg (\ cs opts -> opts { cats = splitCategories . T.pack $ cs })
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
     , Option "s" ["seed"]
            (ReqArg (\ userSeed opts -> opts { seed = read userSeed })
                "SEED")
            "random seed to use"
     ]

argError m = putStrLn helpMessage >> (ioError . userError $ m)

helpMessage = usageInfo "Usage: japex quiz [OPTIONS]" japexOpts

defaultOptions = Options 10 [] "/usr/share/japex/japex.db"

data Options = Options {
                  lineCount :: Int
                , cats :: [T.Text]
                , dbFile :: FilePath
                , seed :: Int
                }

quiz :: [QuizEntry] -> IO [AnswerEntry]
quiz = mapM (\ (Quiz japanese english _) -> do
                    putStrLn . T.unpack $ japanese
                    ua <- getLine
                    return . Answer japanese english $ T.pack ua
            )
