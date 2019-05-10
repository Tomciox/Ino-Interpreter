-------------------------------
-- Autor:       Tomasz Madej --
-- Nr. albumu   385853       --
-------------------------------

{-# LANGUAGE MultiParamTypeClasses, NamedFieldPuns#-}

-- Moduł zawierający interpretery dla poszczególnych kategorii semantycznych programu.

module ExecuteLib where 

import ProgramStateLib

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

-------------------------------------------------------------------------------------------
-- Interpretery programu.
-------------------------------------------------------------------------------------------

executeProgram :: Program -> InterpretMonad ()

-- Funkcja interpretująca wykonanie pustego programu.
executeProgram (ProgramS []) = do
    executeFunction $ Ident "main"

-- Funkcja interpretująca wykonanie niepustego programu.
executeProgram (ProgramS (definition:definitions)) = do
    location <- alloc
    let (FnDef _ ident _ _) = definition in modifyEnvironment (updateEnvironment ident location)
    updateStore location (Right definition)
    executeProgram (ProgramS definitions)

-------------------------------------------------------------------------------------------
-- Interpretery funkcji.
-------------------------------------------------------------------------------------------

executeFunction :: Ident -> InterpretMonad ()

-- Funkcja interpretująca wykonanie funkcji. 
executeFunction ident = do
    object <- getObject ident
    case object of
        -- TODO add function arguments to the block ???
        (Right (FnDef _ _ _ block)) -> do
            executeBlock block
        -- _ -> let (Ident i) = ident in throwError $ "Function `" ++ i ++ "` was not declared in this scope."

-------------------------------------------------------------------------------------------
-- Interpretery bloku.
-------------------------------------------------------------------------------------------

executeBlock :: Block -> InterpretMonad ()

-- TODO scopes ???
-- Funkcja interpretująca wykonanie bloku.
executeBlock (BlockS stmts) = do
    executeStmts stmts

-------------------------------------------------------------------------------------------
-- Interpretery statementów.
-------------------------------------------------------------------------------------------

executeStmts :: [Stmt] -> InterpretMonad ()

-- Funkcja interpretująca wykonanie pustej statementów.
executeStmts [] = do
    return ()

-- Funkcja interpretująca wykonanie niepustej statementów.
executeStmts (stmt:rest) = do
    executeStmt stmt
    executeStmts rest 

executeStmt :: Stmt -> InterpretMonad ()

-- Funkcja interpretująca wykonanie pustego statementu.
executeStmt (Empty) = do
    return ()

-- Funkcja interpretująca wykonanie statementu będącego nowym blokiem.
executeStmt (BStmt (BlockS stmts)) = do
    executeStmts stmts

-- Funkcja interpretująca wykonanie statementu deklaracji.
executeStmt (Decl t declarations) = do
    executeStmtDecls t declarations

-- Funkcja interpretująca wykonanie statementu przypisania.
executeStmt (Ass ident exp) = do
    location <- getLocation ident
    object <- getObject ident
    newValue <- execExpr exp
    -- TODO newVal could be a function ???
    case (object, newValue) of
        (Left (ValueInteger _), ValueInteger _) -> updateStore location (Left newValue)
        (Left (ValueBool _), ValueBool _) -> updateStore location (Left newValue)
        -- TODO print types ???
        _ -> let (Ident i) = ident in throwError $ "Cannot assign to `" ++ i ++ "` an expression of diffrent type."

-- Funkcja interpretująca wykonanie statementu inkrementacji zmiennej.
executeStmt (Incr ident) = do
    location <- getLocation ident
    object <- getObject ident
    case object of
        (Left (ValueInteger value)) -> updateStore location (Left (ValueInteger (value + 1)))
        -- TODO print types ???
        _ -> let (Ident i) = ident in throwError $ "Cannot increment `" ++ i ++ "` which is not of Int type."

-- Funkcja interpretująca wykonanie statementu dekrementacji zmiennej.
executeStmt (Decr ident) = do
    location <- getLocation ident
    object <- getObject ident
    case object of
        (Left (ValueInteger value)) -> updateStore location (Left (ValueInteger (value - 1)))
        -- TODO print types ???
        _ -> let (Ident i) = ident in throwError $ "Cannot decrement `" ++ i ++ "` which is not of Int type."

executeStmtDecls :: Type -> [Item] -> InterpretMonad ()

-- Funkcja interpretująca deklarację pustej listy obiektów.
executeStmtDecls t [] = do
    return ()

-- Funkcja interpretująca deklarację niepustej listy obiektów.
executeStmtDecls t (declaration:declarations) = do
    executeStmtDecl t declaration
    executeStmtDecls t declarations

executeStmtDecl :: Type -> Item -> InterpretMonad ()

-- Funkcja interpretująca deklarację obiektu bez jawnej inicjalizacji.
executeStmtDecl t (NoInit ident) = do
    location <- alloc
    modifyEnvironment (updateEnvironment ident location)
    case t of
        Int -> updateStore location (Left (ValueInteger 0))
        Bool -> updateStore location (Left (ValueBool False))

-- Funkcja interpretująca deklarację obiektu z inicjalizacją.
executeStmtDecl t (Init ident expr) = do
    location <- alloc
    modifyEnvironment (updateEnvironment ident location)
    value <- execExpr expr
    case (t, value) of
        (Int, (ValueInteger _)) -> updateStore location (Left value)
        (Bool, (ValueBool _)) -> updateStore location (Left value)
        _ -> let (Ident i) = ident in throwError $ "Cannot assign to `" ++ i ++ "` an expression which is not of " ++ show t ++ " type."


-------------------------------------------------------------------------------------------
-- Interpretery wyrażeń.
-------------------------------------------------------------------------------------------

execExpr :: Expr -> InterpretMonad Value

-- Funkcja interpretująca wartość liczby całkowitej.
execExpr (ELitInt value) = return $ ValueInteger value

-- Funkcja interpretująca wartość zmiennej.
execExpr (EVar ident) = do
    value <- getObject ident
    case value of
        (Left v) -> return $ v
        -- _ -> let (Ident i) = ident in throwError $ "`" ++ i ++ "` was not declared in this scope."

-- Funkcja interpretująca sumę dwóch wyrażeń.
execExpr (EAdd exp1 op exp2) = do
    val1 <- execExpr exp1
    val2 <- execExpr exp2
    case (val1, val2) of
        (ValueInteger i1, ValueInteger i2) -> case op of
            Plus -> return $ ValueInteger (i1 + i2)
            Minus -> return $ ValueInteger (i1 - i2)
        _ -> throwError $ "Cannot apply any AddOperation to expressions of different types."

-- Funkcja interpretująca iloczyn dwóch wyrażeń.
execExpr (EMul exp1 op exp2) = do
    val1 <- execExpr exp1
    val2 <- execExpr exp2
    case (val1, val2) of
        (ValueInteger i1, ValueInteger i2) -> case op of
            Times -> return $ ValueInteger (i1 * i2)
            Div -> return $ ValueInteger (i1 `div` i2)
            Mod -> return $ ValueInteger (i1 `mod` i2)
        _ -> throwError $ "Cannot apply any MulOperation to expressions of different types."

-- Funkcja interpretująca porównanie dwóch wyrażeń.
execExpr (ERel exp1 op exp2) = do
    val1 <- execExpr exp1
    val2 <- execExpr exp2
    case (val1, val2) of
        (ValueInteger i1, ValueInteger i2) -> case op of
            LTH -> return $ ValueBool (i1 < i2)
            LE -> return $ ValueBool (i1 <= i2)
            GTH -> return $ ValueBool (i1 > i2)
            GE -> return $ ValueBool (i1 >= i2)
            EQU -> return $ ValueBool (i1 == i2)
            NE -> return $ ValueBool (i1 /= i2)
        _ -> throwError $ "Cannot apply RelOperation to expressions of different types."

-- Funkcja interpretująca sumę logiczną dwóch wyrażeń.
execExpr (EAnd exp1 exp2) = do
    val1 <- execExpr exp1
    val2 <- execExpr exp2
    case (val1, val2) of
        (ValueBool i1, ValueBool i2) -> return $ ValueBool (i1 && i2)
        _ -> throwError $ "Cannot apply RelOperation to expressions of different types."

-- Funkcja interpretująca alternatywę logiczną dwóch wyrażeń.
execExpr (EOr exp1 exp2) = do
    val1 <- execExpr exp1
    val2 <- execExpr exp2
    case (val1, val2) of
        (ValueBool i1, ValueBool i2) -> return $ ValueBool (i1 || i2)
        _ -> throwError $ "Cannot apply RelOperation to expressions of different types."

-- Funkcja interpretująca wyrażenie przeciwne.
execExpr (Neg exp) = do
    val <- execExpr exp
    case val of
        ValueInteger i -> return $ ValueInteger (negate i)
        _ -> throwError $ "Cannot apply NegOperation to expression which is not of Int type."

-- Funkcja interpretująca negację wyrażenia.
execExpr (Not exp) = do
    val <- execExpr exp
    case val of
        ValueBool i -> return $ ValueBool (not i)
        _ -> throwError $ "Cannot apply NotOperation to expression which is not of Bool type."
    
-- Funkcja interpretująca literał "prawda".
execExpr (ELitTrue) = do
    return $ ValueBool True

-- Funkcja interpretująca literał "fałsz".
execExpr (ELitFalse) = do
    return $ ValueBool False