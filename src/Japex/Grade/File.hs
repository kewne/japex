module Japex.Grade.File
    (
        generateGradeFileName
        , writeGradeFile
        , findResultFiles
        , transformResultsFile
    ) where

import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.IO as TIo
import Japex.Common
import Japex.Grade.Common
import Japex.Quiz.Common
import Japex.Quiz.File(getUserQuizDir)
import System.Directory
import System.FilePath

generateGradeFileName resultFileName = liftM (</> resultFileName) getUserGradeSubDir

getUserGradeSubDir = getJapexUserDataSubDir "grade"

writeGradeFile file stats = do
    gradeFileName <- generateGradeFileName . takeFileName $ file
    TIo.writeFile gradeFileName $ format stats

format = T.unlines . map (T.intercalate (T.singleton ':') . reviewToLine)
    where reviewToLine (a,b,c) = [a,b,c]

findResultFiles = do
    quizSubDir <- getUserQuizDir
    quizSubDirContents <- getDirectoryContents quizSubDir
    let resultFiles = map (quizSubDir </>) . filter (`notElem` [".", ".."]) $ quizSubDirContents
    when (null resultFiles) $ fail "No result files found"
    return resultFiles
    
readResultsFile file = TIo.readFile file >>= \ contents -> return . parseResults $ contents

parseResults = map (toAnswer . T.split (==':')) . T.lines
    where toAnswer [a,b,c] = Answer a b c

transformResultsFile file f = do
    results <- readResultsFile file
    stats <- f results
    writeGradeFile file stats
    removeFile file
