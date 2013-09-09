module Japex.Common
    (
        Command(Command, commandFunc, helpFunc)
        , getJapexUserDataSubDir
    ) where

import System.FilePath
import System.Environment(getProgName)
import System.Directory(
    createDirectoryIfMissing
    , getAppUserDataDirectory)
                           
data Command = Command {
        commandFunc :: [String] -> IO()
        , helpFunc :: IO ()
    }

getJapexUserDataSubDir subdir = do
    progName <- getProgName
    appDataDir <- getAppUserDataDirectory progName
    let subDirPath = joinPath [appDataDir, subdir]
    createDirectoryIfMissing True subDirPath
    return subDirPath
