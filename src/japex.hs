import Data.List
import Japex.Common
import Japex.Quiz
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import System.Random


commands = [ ("quiz", doQuiz)
           , ("grade", doGrade)
           , ("review", doReview)
           ]

main = do
    args <- getArgs
    doCommand args

doCommand (command : args) = maybe noCommand ($ args) com
    where com = lookup command commands
          noCommand = ioError $ userError  $ "No such command: " ++ command

doCommand [] = ioError $ userError "No command given"
        
doGrade args = ioError $ userError "grading not implemented"

doReview args = ioError $ userError "reviewing not implemented"

