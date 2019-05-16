-------------------------------
-- Autor:       Tomasz Madej --
-- Nr. albumu   385853       --
-------------------------------

module TupleHelper where

import InterpreterStateLib

import LexIno
import ParIno
import SkelIno
import PrintIno
import AbsIno

import qualified Data.Map as Map
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Identity
import Data.Maybe(catMaybes)

-- Funkcja zwracająca tuplę, po zmienieniu wartości zadanego jej elementu.
assignTuple :: Value -> [Integer] -> Value -> InterpretMonad Value

assignTuple tuple [] newValue = 
    case getValueType tuple == getValueType newValue of
        True -> return newValue
        False -> throwError $ "Ino Exception: Tuple type mismatch (assign)."

assignTuple tuple (indice:indices) newValue = 
    case tuple of
        (ValueTuple vals) -> case (length vals) >= fromInteger indice of
            True -> let 
                prefix = take (fromInteger (indice - 1)) vals
                (value:suffix) = drop (fromInteger (indice -1)) vals 
                in do
                    new <- assignTuple value indices newValue
                    return $ ValueTuple (prefix ++ (new:suffix))
            False -> throwError $ "Ino Exception: Tuple indice out of range (assign)."
        _ -> throwError $ "Ino Exception: Tuple has not enough dimensions (assign)."

-- Funkcja wyłuskująca zadany element tupli.
getTuple :: Value -> [Integer] -> InterpretMonad Value

getTuple tuple [] = do
    return tuple

getTuple tuple (indice:indices) =
    case tuple of
        (ValueTuple vals) -> case (length vals) >= fromInteger indice of
            True -> let 
                value = head $ drop (fromInteger (indice -1)) vals 
                in do
                    new <- getTuple value indices
                    return $ new
            False -> throwError $ "Ino Exception: Tuple indice out of range (get)."
        _ -> throwError $ "Ino Exception: Tuple has not enough dimensions (get)."