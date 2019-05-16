-------------------------------
-- Autor:       Tomasz Madej --
-- Nr. albumu   385853       --
-------------------------------

module TupleHelper where

import InterpreterStateLib as ISL
import TypeCheckerStateLib as TCSL


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
        False -> throwError $ "Ino Exception: Tuple assignment type incompatibility."

assignTuple tuple (indice:indices) newValue = 
    case tuple of
        (ValueTuple vals) -> case (length vals) >= fromInteger indice of
            True -> let 
                prefix = take (fromInteger (indice - 1)) vals
                (value:suffix) = drop (fromInteger (indice - 1)) vals 
                in do
                    new <- assignTuple value indices newValue
                    return $ ValueTuple (prefix ++ (new:suffix))
            False -> throwError $ "Ino Exception: Tuple assignment indice out of range."
        _ -> throwError $ "Ino Exception: Tuple assignment has not enough dimensions."

-- Funkcja wyłuskująca zadany element tupli.
getTuple :: Value -> [Integer] -> InterpretMonad Value

getTuple tuple [] =
    return tuple

getTuple tuple (indice:indices) =
    case tuple of
        (ValueTuple vals) -> case (length vals) >= fromInteger indice of
            True -> let 
                value = head $ drop (fromInteger (indice - 1)) vals 
                in getTuple value indices
            False -> throwError $ "Ino Exception: Tuple get indice out of range."
        _ -> throwError $ "Ino Exception: Tuple has not enough dimensions to get."

-- Funkcja sprawdzająca czy można przypisać wartość zadanemu fragmentowi tupli
typeCheckAssignTuple :: Type -> [Integer] -> Type -> TypeCheckMonad ()

typeCheckAssignTuple tuple [] t = 
    case (tuple == t) of
        True -> return ()
        False -> throwError $ "Ino TypeChecker Exception: Tuple assignment type incompatibility."

typeCheckAssignTuple tuple (indice:indices) t = 
    case tuple of
        (Tuple ts) -> case (length ts) >= fromInteger indice of
            True -> let 
                tuple2 = head $ drop (fromInteger (indice - 1)) ts
                in typeCheckAssignTuple tuple2 indices t
            False -> throwError $ "Ino TypeChecker Exception: Tuple assignment indice out of range."
        _ -> throwError $ "Ino TypeChecker Exception: Tuple assignment has not enough dimensions."

-- Funkcja wyłuskująca typ zadanego elementu z tupli.
typeCheckGetTuple :: Type -> [Integer] -> TypeCheckMonad Type

typeCheckGetTuple tuple [] =
    return tuple

typeCheckGetTuple tuple (indice:indices) =
    case tuple of
        (Tuple ts) -> case (length ts) >= fromInteger indice of
            True -> let 
                tuple2 = head $ drop (fromInteger (indice - 1)) ts 
                in typeCheckGetTuple tuple2 indices 
            False -> throwError $ "Ino TypeChecker Exception: Tuple get indice out of range."
        _ -> throwError $ "Ino TypeChecker Exception: Tuple get has not enough dimensions."