module Japex.Help
    (helpCommand)
where

import Japex.Common

helpCommand commands = Command (doHelp commands) printHelp

helpMessages = [
            ("quiz", "Generates a quiz")
            , ("grade", "Grades a set of quiz results")
            , ("review", "Reviews grading results")
            , ("help", "Displays this help message")
            ]

doHelp commands [comm] = maybe noCommand helpFunc $ lookup comm commands
    where noCommand = ioError $ userError $ "Command " ++ comm ++ " does not exist"
doHelp commands [] = putStrLn . unlines . map printCom $ helpMessages
    where printCom (name, desc) = name ++ "\t" ++ desc

printHelp = putStrLn "help"
