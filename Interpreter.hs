-------------------------------
-- Autor:       Tomasz Madej --
-- Nr. albumu   385853       --
-------------------------------

import System.Environment(getArgs)
import System.IO(stdin, hGetContents)
import System.Environment(getArgs, getProgName)
import System.Exit(exitFailure, exitSuccess)

import LexIno
import ParIno
import SkelIno
import PrintIno
import AbsIno

import InterpreterLib

import ErrM

type ParseFun a = [Token] -> Err a

myLLexer = myLexer

runFile :: ParseFun Program -> FilePath -> IO ()
runFile p f = readFile f >>= runProgram p

runProgram :: ParseFun Program -> String -> IO ()
runProgram p s = let ts = myLLexer s in case p ts of
    (Bad s) -> do 
        putStrLn "Parse Failed...\n"
        putStrLn s
        exitFailure
    (Ok tree) -> do 
        putStrLn "Parse Successful!\n"
        interpret tree
        exitSuccess

main :: IO ()
main = do
    args <- getArgs
    case args of
        [x] -> runFile pProgram x 
        _ -> putStrLn "Invalid number of args"