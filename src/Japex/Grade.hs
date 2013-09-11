module Japex.Grade
    (
        gradeCommand
    ) where

import Control.Monad
import System.Directory
import System.FilePath
import System.IO
import Data.List
import Japex.Common

data Options = Options {
    resultFile :: FilePath
    }

gradeCommand = Command doGrade (putStrLn "grade")

defaultOptions = Options "dummy"

doGrade args = do
    opts <- parseArgs args defaultOptions
    results <- readFile . resultFile $ opts
    stats <- grade . parseResults $ results
    gradeFileName <- generateGradeFileName . takeFileName . resultFile $ opts
    writeFile gradeFileName $ format stats
    removeFile . resultFile $ opts

parseArgs as defs = do
    resultFiles <- findResultFiles
    let opts = defs { resultFile = selectResultFile resultFiles }
    putStrLn . resultFile $ opts
    return opts

selectResultFile = minimum

findResultFiles = do
    quizSubDir <- getJapexUserDataSubDir "quiz"
    quizSubDirContents <- getDirectoryContents quizSubDir
    let resultFiles = map (joinPath . ([quizSubDir] ++) . (:[])) . filter (`notElem` [".", ".."]) $ quizSubDirContents
    when (null resultFiles) $ fail "No result files found"
    return resultFiles
    
generateGradeFileName resultFileName = do
    gradeSubDir <- getJapexUserDataSubDir "grade"
    return $ joinPath [ gradeSubDir, resultFileName ]

format = unlines . map (intercalate ":" . tupleToArray)
    where tupleToArray (a,b,c) = [a,b,c]

parseResults ls = map extractFields $ lines ls

extractFields l = (q,ca,a)
    where (q,as) = breakField l
          (ca,a') = breakField $ tail as
          a = tail a'
          breakField = break (== ':')

grade = mapM gradeAnswer

gradeAnswer (q, ca, a) 
    | ca == a = do
        putStrLn "Answer matched correct one exactly, skipping..."
        return (q, a, a)
    | otherwise = do
        putStrLn $ "Is " ++ q ++ " = '" ++ a ++ "'? [" ++ ca ++"] (Y/n)"
        g <- getLine
        requestCorrection g q a ca

requestCorrection g q a ca
    | g == "y" || g == "" = return (q,a,a)
    | otherwise = do
        ca <- correct ca
        return (q,a, ca)

correct a = do
    putStrLn $ "Please write the correct answer (" ++ a ++ ")"
    getLine
