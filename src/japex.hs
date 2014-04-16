import Data.List
import Japex.Common
import Japex.Grade
import Japex.Quiz
import Japex.Help
import Japex.Numbers
import System.Environment(getArgs)
import System.Exit
import System.IO


commands = [ ("vocabulary", quizCommand)
           , ("help", helpCommand commands)
           , ("numbers", numbersCommand)
           ]

main = do
    args <- getArgs
    doCommand args

doCommand [] = do
    commandFunc (helpCommand commands) []
    ioError $ userError "No command given"
doCommand (command : args) = maybe noCommand (executeCommand args) com
    where com = lookup command commands
          noCommand = ioError $ userError  $ "No such command: " ++ command
          executeCommand args com = commandFunc com args
