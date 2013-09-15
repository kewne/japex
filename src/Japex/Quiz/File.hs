module Japex.Quiz.File
    (
        readQuizDatabaseFile
        , generateResultFileName
        , writeAnswerFile
        , getUserQuizDir
    ) where

import Control.Monad
import Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as TIo
import Data.Time.Clock
import Data.Time.Format
import Japex.Common
import Japex.Quiz.Common
    (
        QuizEntry(Quiz)
        , AnswerEntry(Answer)
        , splitCategories
    )
import System.FilePath
import System.Locale

readQuizDatabaseFile :: FilePath -> IO [QuizEntry]
readQuizDatabaseFile = liftM extractExercises . TIo.readFile

extractExercises :: T.Text -> [QuizEntry]
extractExercises = map splitFields . T.lines
    where splitFields = toQuiz . T.split (==':')
          toQuiz [a,b,c] = Quiz a b (splitCategories c)

getUserQuizDir = getJapexUserDataSubDir "quiz"

writeAnswerFile :: [AnswerEntry] -> IO ()
writeAnswerFile answers = (`TIo.writeFile` toAnswerString answers) =<< generateResultFileName

generateResultFileName = do
    quizSubDir <- getUserQuizDir
    liftM ((quizSubDir </>) . formatTime defaultTimeLocale "%s") getCurrentTime

toAnswerString :: [AnswerEntry] -> T.Text
toAnswerString = T.unlines . map (\ (Answer q a ua) -> T.intercalate (T.singleton ':') [q,a,ua])
