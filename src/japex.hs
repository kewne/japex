import Data.List
import Japex.Common
import Japex.Grade
import Japex.Quiz
import Japex.Review
import Japex.Help
import System.Environment
import System.Exit
import System.IO
import System.Random


commands = [ ("quiz", quizCommand)
           , ("grade", gradeCommand)
           , ("review", reviewCommand)
           , ("help", helpCommand commands)
           ]

main = do
    args <- getArgs
    doCommand args

doCommand (command : args) = maybe noCommand (executeCommand args) com
    where com = lookup command commands
          noCommand = ioError $ userError  $ "No such command: " ++ command

doCommand [] = ioError $ userError "No command given"

executeCommand args com = commandFunc com $ args
