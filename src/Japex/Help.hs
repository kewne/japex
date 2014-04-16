module Japex.Help
    (helpCommand)
where

import Japex.Common
import Paths_japex

helpCommand commands = Command (doHelp commands) printHelp

helpMessages = [
            ("vocabulary", "Generates a quiz")
            , ("numbers", "Generates number sequences to spell out")
            , ("help", "Displays this help message")
            ]

doHelp commands [comm] = maybe noCommand helpFunc $ lookup comm commands
    where noCommand = ioError $ userError $ "Command " ++ comm ++ " does not exist"
doHelp commands [] = putStrLn . unlines . map printCom $ helpMessages
    where printCom (name, desc) = name ++ "\t" ++ desc

printHelp = putStrLn $ unlines [
    "Lists available commands"
    , "\tjapex help"
    , ""
    , "Prints help about a command"
    , "\tjapex help COMMAND"
    ]
