import Data.List
import Japex.Common
import Japex.Quiz
import Japex.Help
import Japex.Numbers
import System.Environment(getArgs)
import System.Exit
import System.IO

availableCommands = [
	quizCommand
	, numbersCommand
	]

helpForCommands = helpCommand availableCommands

main = do
    args <- getArgs
    doCommand args

doCommand [] = do
    run (helpCommand availableCommands) []
    ioError $ userError "No command given"
doCommand (command : args) = maybe noCommand (`run` args) (findByName (helpForCommands : availableCommands) command)
    where noCommand = ioError $ userError  $ "No such command: " ++ command
