import Data.List
import Japex.Common
import Japex.Grade
import Japex.Quiz
import Japex.Review
import System.Environment
import System.Exit
import System.IO
import System.Random


commands = [ ("quiz", doQuiz)
           , ("grade", doGrade)
           , ("review", doReview)
           , ("help", doHelp)
           ]
helpMessages = [
            ("quiz", "Generates a quiz")
            , ("grade", "Grades a set of quiz results")
            , ("review", "Reviews grading results")
            , ("help", "Displays this help message")
            ]

main = do
    args <- getArgs
    doCommand args

doCommand (command : args) = maybe noCommand ($ args) com
    where com = lookup command commands
          noCommand = ioError $ userError  $ "No such command: " ++ command

doCommand [] = ioError $ userError "No command given"

doHelp args = putStrLn . unlines . map printCom $ helpMessages
    where printCom (name, desc) = name ++ "\t" ++ desc
