module Japex.Quiz.File
    (
        extractExercises
        , generateResultFileName
        , writeAnswerFile
        , getUserQuizDir
    ) where

import Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as TIo
import Data.Time.Clock
import Data.Time.Format
import Japex.Common
import Japex.Quiz.Common
    (
        QuizEntry(Quiz)
        , splitCategories
    )
import System.FilePath
import System.Locale

extractExercises :: FilePath -> IO [QuizEntry]
extractExercises f = do
    fileContents <- TIo.readFile f
    return . map splitFields . T.lines $ fileContents

splitFields l = Quiz jap eng cats
    where [jap, eng, unsplitCats] = take 3 . T.split (==':') $ l
          cats = splitCategories unsplitCats

generateResultFileName = do
    quizSubDir <- getUserQuizDir
    now <- getCurrentTime
    return $ joinPath [quizSubDir, formatTime defaultTimeLocale "%s" now]

getUserQuizDir = getJapexUserDataSubDir "quiz"

toAnswerString = T.unlines . map (\ (q,a,ua) -> T.intercalate (T.singleton ':') [q,a,ua])

writeAnswerFile answers = do
    resultFileName <- generateResultFileName
    TIo.writeFile resultFileName (toAnswerString answers)
