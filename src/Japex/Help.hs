module Japex.Help
    (helpCommand)
where

import Japex.Common
import Paths_japex

helpCommand commands = Command (doHelp commands) "help" "Prints help" printHelp

doHelp :: [JapexCommand] -> [String] -> IO ()
doHelp commands ["help"] = putStrLn printHelp
doHelp commands [comm] = maybe noCommand (putStrLn . help) (findByName commands comm)
    where noCommand = ioError $ userError $ "Command " ++ comm ++ " does not exist"
doHelp commands [] = putStrLn . unlines . map (\c -> name c ++ "\t" ++ shortHelp c) $ commands

printHelp = unlines [
    "Lists available commands"
    , "\tjapex help"
    , ""
    , "Prints help about a command"
    , "\tjapex help COMMAND"
    ]
