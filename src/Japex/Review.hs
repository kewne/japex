module Japex.Review
    (reviewCommand)
where

import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.IO as TIo
import Japex.Common
import Japex.Grade.File
import System.Directory
import System.FilePath
import System.IO

reviewCommand = Command doReview (putStrLn "review")

doReview args = do
    transformGradeFile review =<< (liftM minimum findGradeFiles)

transformGradeFile f file = do
    grades <- readGradeFile file
    let reviewedResults = f grades
    writeReviewFile file reviewedResults
    putStrLn . summarize $ reviewedResults
    removeFile file

findGradeFiles :: IO [FilePath]
findGradeFiles = do
    userGradeDir <- getUserGradeSubDir
    userGradeDirContents <- getDirectoryContents userGradeDir
    let gradeFiles = map (userGradeDir </>) . filter (`notElem` [".", ".."]) $ userGradeDirContents
    when (null gradeFiles) $ fail "No grade files found"
    return gradeFiles

readGradeFile :: FilePath -> IO [AnswerEntry]
readGradeFile = liftM parseGrades . TIo.readFile

parseGrades = map extractFields . T.lines
    where extractFields = toGrade . T.split (==':')
          toGrade [a,b,c] = Grade a b c

review :: [AnswerEntry] -> [(T.Text, T.Text, T.Text, Bool)]
review = map reviewSingle
    where reviewSingle (Grade q a ca) = (q,a,ca, a == ca)

writeReviewFile file results = do
    userReviewDir <- getJapexUserDataSubDir "review"
    TIo.writeFile ((userReviewDir </>) . takeFileName $ file) (format results)

format = T.unlines . map resultToString
    where resultToString (q, a, ca, c)
            | c = T.concat [q, T.pack " correct, is ", a]
            | otherwise = T.concat [q, T.pack " incorrect, guessed ", a, T.pack ", is ", ca]
          
summarize rs = show a ++ "/" ++ show b ++ " correct answers"
    where (a, b) = foldr accumCorrect (0, 0) rs
          accumCorrect (_,_,_,c) (ct,t)
            | c = (ct + 1, t + 1)
            | otherwise = (ct, t + 1)
