module Japex.Grade
    (
        doGrade
    ) where

import System.IO
import Data.List

doGrade args = do
            resFile <- parseArgs args
            results <- readFile resFile
            stats <- grade . parseResults $ results
            writeFile "grades.txt" $ format stats

format = unlines . map (intercalate ":" . tupleToArray)
    where tupleToArray (a,b,c) = [a,b,c]

parseArgs [] = ioError $ userError "No results file given"
parseArgs as = return $ head as

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
                        putStrLn $ "Is " ++ q ++ " = " ++ a ++ " ? (y/n)"
                        g <- getLine
                        requestCorrection g q a ca

requestCorrection g q a ca
    | g == "y" = return (q,a,a)
    | otherwise = do
                    ca <- correct ca
                    return (q,a, ca)

correct a = do
    putStrLn $ "Please write the correct answer (" ++ a ++ ")"
    getLine
