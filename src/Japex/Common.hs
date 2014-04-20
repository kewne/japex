module Japex.Common
    (
        JapexCommand(..)
        , findByName
        , QuizEntry(..)
        , getJapexUserDataSubDir
    ) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIo
import Data.List
import System.FilePath
import System.Environment(getProgName)
import System.Directory

data JapexCommand = Command {
		run :: [String] -> IO ()
		, name :: String
		, shortHelp :: String
		, help :: String
	}

findByName :: [JapexCommand] -> String -> Maybe JapexCommand
findByName commands comName = find ((== comName) . name) commands

data QuizEntry = Quiz {
    japanese :: T.Text
    , english :: T.Text
    , categories :: [T.Text]
} deriving (Show)

getJapexUserDataSubDir subdir = do
    progName <- getProgName
    appDataDir <- getAppUserDataDirectory progName
    let subDirPath = joinPath [appDataDir, subdir]
    createDirectoryIfMissing True subDirPath
    return subDirPath
