-------------------------------
-- Autor:       Tomasz Madej --
-- Nr. albumu   385853       --
-------------------------------

module InterpreterLib where

import ExecuteLib
import ProgramStateLib

import LexIno
import ParIno
import SkelIno
import PrintIno
import AbsIno

import qualified Data.Map as Map

-------------------------------------------------------------------------------------------
-- Funkcja uruchamiajaca interpretację zadanego programu.
-------------------------------------------------------------------------------------------

runProgram :: Program -> IO ()
runProgram program = do
    result <- runInterpretMonad (executeProgram program) initialState
    case result of
        Left error -> do
            putStrLn error
        Right (_, state) -> do
            print $ Map.toAscList (environment state)
            print $ Map.toAscList (store state)