
module Main where

import System.Environment
import System.Directory
import Parser
import StackInterpreter




main = do
    cliArgs <- getArgs
    case cliArgs of
        [fileName] -> runInterpreter fileName
        [] -> error "Wrong format: Expected 1 argument but get 0.\nUsage: ./stk <filename>"
        _  -> error "Wrong format: Expected 1 argument but get more than one.\nUsage: ./stk <filename>"

split :: String -> [[String]]
split s = map words $ lines s

runInterpreter :: String -> IO ()
runInterpreter fileName = do
    ex <- doesFileExist fileName
    if not $ ex 
        then
            error $ (show fileName) ++ " is not a file.\nUsage: ./stk <filename>"
        else do
            programText <- readFile fileName
            let tokens = split programText
                (instructions, labels) = parse tokens
            runProgram instructions labels 
