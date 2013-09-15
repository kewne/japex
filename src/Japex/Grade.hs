module Japex.Grade
    (
        gradeCommand
    ) where

import Control.Monad
import System.IO
import Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as TIo
import Japex.Common
import Japex.Grade.File
import Japex.Quiz.Common
import Japex.Quiz.File(getUserQuizDir)

gradeCommand = Command doGrade (putStrLn "grade")

data Options = Options {
    resultFile :: FilePath
    }

defaultOptions = Options "dummy"

doGrade args = do
    opts <- parseArgs args
    transformResultsFile (resultFile opts) grade

parseArgs as = do
    resultFiles <- findResultFiles
    return $ defaultOptions { resultFile = selectResultFile resultFiles }

selectResultFile = minimum

grade = mapM gradeAnswer

gradeAnswer (Answer q ca a)
    | ca == a = do
        putStrLn "Answer matched correct one exactly, skipping..."
        return (q, a, a)
    | otherwise = do
        TIo.putStrLn . T.concat $ [T.pack "Is ", q, T.pack " = '", a, T.pack "'? [", ca, T.pack"] (Y/n)"]
        g <- TIo.getLine
        requestCorrection g q a ca

requestCorrection g q a ca
    | g == T.singleton 'y' || T.null g = return (q,a,a)
    | otherwise = do
        ca <- correct ca
        return (q,a, ca)

correct a = do
    TIo.putStrLn . T.concat $ [T.pack "Please write the correct answer (", a, T.pack ")"]
    TIo.getLine
