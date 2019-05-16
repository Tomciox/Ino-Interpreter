-------------------------------
-- Autor:       Tomasz Madej --
-- Nr. albumu   385853       --
-------------------------------

import System.Environment(getArgs)
-- import System.IO(stdin, hGetContents)
import System.Environment(getArgs, getProgName)
import System.Exit(exitFailure, exitSuccess)

import LexIno
import ParIno
import SkelIno
import PrintIno
import AbsIno

import InterpreterStateLib as ISL
import InterpreterLib
import TypeCheckerStateLib as TCSL
import TypeCheckerLib

import ErrM

type ParseFun a = [Token] -> Err a

myLLexer = myLexer

-------------------------------------------------------------------------------------------
-- Funkcja uruchamiajaca statyczne typowanie w zadanym programie.
-------------------------------------------------------------------------------------------

typeCheck :: Program -> IO ()
typeCheck program = do
    result <- runTypeCheckMonad (typeCheckProgram program) TCSL.initialState
    case result of
        Left error -> do
            putStrLn $ "\n" ++ error
            exitFailure
        Right (_, state) -> do
            putStrLn "\nIno TypeChecker succeeded."

-------------------------------------------------------------------------------------------
-- Funkcja uruchamiajaca interpretację zadanego programu.
-------------------------------------------------------------------------------------------

interpret :: Program -> IO ()
interpret program = do
    result <- runInterpretMonad (executeProgram program) ISL.initialState
    case result of
        Left error -> do
            putStrLn $ "\n" ++ error
        Right (_, state) -> do
            -- print $ Map.toAscList (environment state)
            -- print $ Map.toAscList (store state)
           putStrLn "\nIno Interpreter succeeded."

runFile :: ParseFun Program -> FilePath -> IO ()
runFile p f = readFile f >>= runProgram p

runProgram :: ParseFun Program -> String -> IO ()
runProgram p s = let ts = myLLexer s in case p ts of
    (Bad s) -> do 
        putStrLn "Parse Failed..."
        putStrLn s
        exitFailure
    (Ok program) -> do 
        putStrLn "Parse Successful."
        typeCheck program
        interpret program
        exitSuccess

main :: IO ()
main = do
    args <- getArgs
    case args of
        [x] -> runFile pProgram x 
        _ -> putStrLn "Invalid number of args"
