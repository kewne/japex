module Japex.Quiz.File
    (
        extractExercises
        , generateResultFileName
        , writeAnswerFile
    ) where

import Data.List
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
    fileContents <- readFile f
    return . map splitFields . lines $ fileContents

splitFields l = Quiz jap eng cats
    where (jap, engAndCats) = breakField l
          (eng, unsplitCats) = breakField . tail $ engAndCats
          cats = splitCategories . tail $ unsplitCats
          breakField = break (== ':')

generateResultFileName = do
    quizSubDir <- getUserQuizDir
    now <- getCurrentTime
    return $ joinPath [quizSubDir, formatTime defaultTimeLocale "%s" now]

getUserQuizDir = getJapexUserDataSubDir "quiz"

toAnswerString = unlines . map (\ (q,a,ua) -> intercalate ":" [q,a,ua])

writeAnswerFile answers = do
    resultFileName <- generateResultFileName
    writeFile resultFileName (toAnswerString answers)
