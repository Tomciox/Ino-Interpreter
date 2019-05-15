-------------------------------
-- Autor:       Tomasz Madej --
-- Nr. albumu   385853       --
-------------------------------

module PrintHelper where 

import ProgramStateLib

import LexIno
import ParIno
import SkelIno
import PrintIno
import AbsIno

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Identity

-- Jawne wypisanie listy wartości.
printValues :: [Value] -> InterpretMonad ()

printValues [] = 
    return ()

printValues (v:[]) = do
    printValue v

printValues (v:vs) = do
    printValue v
    liftIO $ putStr ", "
    printValues vs

-- Jawne wypisanie pojedynczej wartości.
printValue :: Value -> InterpretMonad ()

printValue v = case v of
    (ValueInteger i) -> do
        liftIO $ putStr $ show i
    (ValueBool b) -> do
        liftIO $ putStr $ show b
    (ValueVoid) -> do
        liftIO $ putStr "()"
    (ValueString s) -> do
        liftIO $ putStr s
    (ValueTuple values) -> do
        liftIO $ putStr "("
        printValues values
        liftIO $ putStr ")"
