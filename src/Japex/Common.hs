module Japex.Common
    (
        Command(Command, commandFunc, helpFunc)
        , QuizEntry(..)
        , AnswerEntry(..)
        , CorrectedEntry(..)
        , formatCorrected
        , getJapexUserDataSubDir
        , interactiveTransformFile
        , transformFile
    ) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIo
import System.FilePath
import System.Environment(getProgName)
import System.Directory
                           
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
}

parseAnswer :: T.Text -> AnswerEntry
parseAnswer = toAnswer . T.split (==':')
    where toAnswer [a,b,c] = Answer a b c

formatAnswer :: AnswerEntry -> T.Text
formatAnswer (Answer a b c) = T.intercalate (T.singleton ':') $ [a,b,c]

data CorrectedEntry = Correct Bool AnswerEntry

formatCorrected :: CorrectedEntry -> T.Text
formatCorrected (Correct True a) = T.concat [T.pack "correct:", formatAnswer a]
formatCorrected (Correct False a) = T.concat [T.pack "incorrect:", formatAnswer a]

getJapexUserDataSubDir subdir = do
    progName <- getProgName
    appDataDir <- getAppUserDataDirectory progName
    let subDirPath = joinPath [appDataDir, subdir]
    createDirectoryIfMissing True subDirPath
    return subDirPath

transformFile :: (FilePath -> FilePath) -> (T.Text -> T.Text) -> FilePath -> IO ()
transformFile fileF textF file = do
    inFileContents <- TIo.readFile file
    TIo.writeFile (fileF file) (textF inFileContents)
    removeFile file

interactiveTransformFile :: (FilePath -> FilePath) -> (T.Text -> IO T.Text) -> FilePath -> IO ()
interactiveTransformFile fileF textF file = do
    inFileContents <- TIo.readFile file
    outFileContents <- textF inFileContents
    TIo.writeFile (fileF file) outFileContents
    removeFile file
