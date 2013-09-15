module Japex.Common
    (
        Command(Command, commandFunc, helpFunc)
        , QuizEntry(..)
        , AnswerEntry(..)
        , getJapexUserDataSubDir
    ) where

import qualified Data.Text as T
import System.FilePath
import System.Environment(getProgName)
import System.Directory(
    createDirectoryIfMissing
    , getAppUserDataDirectory)
                           
data Command = Command {
        commandFunc :: [String] -> IO()
        , helpFunc :: IO ()
    }

data QuizEntry = Quiz {
    japanese :: T.Text
    , english :: T.Text
    , categories :: [T.Text]
}

data AnswerEntry = Answer {
    question :: T.Text
    , userAnswer :: T.Text
    , correctAnswer :: T.Text
} | Grade {
    question :: T.Text
    , answer :: T.Text
    , correctAnswer :: T.Text
}

getJapexUserDataSubDir subdir = do
    progName <- getProgName
    appDataDir <- getAppUserDataDirectory progName
    let subDirPath = joinPath [appDataDir, subdir]
    createDirectoryIfMissing True subDirPath
    return subDirPath
