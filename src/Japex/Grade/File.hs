module Japex.Grade.File
    (
        getUserGradeSubDir
        , findResultFiles
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

getUserGradeSubDir = getJapexUserDataSubDir "grade"

findResultFiles = do
    quizSubDir <- getUserQuizDir
    quizSubDirContents <- getDirectoryContents quizSubDir
    let resultFiles = map (quizSubDir </>) . filter (`notElem` [".", ".."]) $ quizSubDirContents
    when (null resultFiles) $ fail "No result files found"
    return resultFiles

