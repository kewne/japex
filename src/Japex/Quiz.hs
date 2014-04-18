module Japex.Quiz (
      quizCommand
    ) where

import Control.Monad
import Data.List
import Japex.Common
import Japex.Quiz.Common
import Japex.Quiz.File
import Paths_japex
import System.Console.GetOpt
import System.Environment(getProgName)
import System.Exit
import System.IO
import System.Random
import qualified Data.Text as T
import qualified Data.Text.IO as TIo

quizCommand = Command doQuiz "vocabulary" "Exercises vocabulary" helpMessage

doQuiz :: [String] -> IO ()
doQuiz args = do
    seed <- randomIO
    defaultDatabase <- getDataFileName "database.txt"
    let defaults = Options 10 [] defaultDatabase seed
    either (ioError . userError) runQuiz $ processArgs args defaults

runQuiz :: Options -> IO()
runQuiz os = do
    questions <- readQuizDatabaseFile $ dbFile os
    quiz $ selectExs os questions
    
selectExs :: Options -> [QuizEntry] -> [QuizEntry]
selectExs (Options lineCount cats _ seed) = take lineCount . shuffle seed . filterByCats cats

shuffle :: Int -> [QuizEntry] -> [QuizEntry]
shuffle seed exs = map (exs !!) $ randomRs range $ mkStdGen seed
    where range = (0, length exs -1)

filterByCats :: [T.Text] -> [QuizEntry] -> [QuizEntry]
filterByCats cats = filter (null . (cats \\ ) . categories)
        
processArgs args defaults =
    case getOpt Permute japexOpts args of
        (o,_,[]) -> Right $ foldr id defaults o
        (_,_,e) -> Left $ concat e

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

helpMessage = usageInfo "Usage: japex vocabulary [OPTIONS]" japexOpts

data Options = Options {
                  lineCount :: Int
                , cats :: [T.Text]
                , dbFile :: FilePath
                , seed :: Int
                }

quiz :: [QuizEntry] -> IO ()
quiz = mapM_ (\ (Quiz japanese english _) -> do
                    putStrLn $ T.unpack japanese
                    getLine
                    putStrLn ("(" ++ T.unpack english ++ ")\n")
            )
