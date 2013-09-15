module Japex.Grade
    (
        gradeCommand
    ) where

import Control.Monad
import Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as TIo
import Japex.Common
import Japex.Grade.File
import Japex.Quiz.Common
import Japex.Quiz.File(getUserQuizDir)
import System.FilePath
import System.IO

gradeCommand = Command doGrade (putStrLn "grade")

data Options = Options {
    resultFile :: FilePath
    }

defaultOptions = Options "dummy"

doGrade args = do
    resultFile <- liftM selectResultFile findResultFiles
    userGradeDir <- getUserGradeSubDir
    interactiveTransformFile ((userGradeDir </>) . takeFileName) (liftM format . grade . parseResults) resultFile

selectResultFile = minimum

format :: [CorrectedEntry] -> T.Text
format = T.unlines . map formatCorrected

parseResults :: T.Text -> [AnswerEntry]
parseResults = map (toAnswer . T.split (==':')) . T.lines
    where toAnswer [a,b,c] = Answer a b c

grade :: [AnswerEntry] -> IO [CorrectedEntry]
grade = mapM gradeAnswer 

gradeAnswer :: AnswerEntry -> IO CorrectedEntry
gradeAnswer a
    | isExactMatch a = do
        putStrLn "Answer matched correct one exactly, skipping..."
        return $ Correct True a
    | otherwise = confirmIncorrect a

isExactMatch (Answer q a ca) = a == ca

confirmIncorrect a = do
        TIo.putStrLn . T.concat $ [T.pack "Is ", question a, T.pack " = '", userAnswer a, T.pack "'? [", correctAnswer a, T.pack"] (Y/n)"]
        g <- TIo.getLine
        return $ Correct (g == T.singleton 'y' || T.null g) a
