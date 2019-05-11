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
    object <- getObject (Ident "main")
    case object of
        (Right (FnDef Int _ [] _)) -> do
            retVal <- executeFunction (Ident "main") []
            case retVal of
                (ValueInteger 0) -> return ()
                (ValueInteger r) -> throwError $ "Program failed with exit code " ++ show r ++ "."
        _ -> throwError $ "Main function is not defined."

-- Funkcja interpretująca wykonanie niepustego programu.
executeProgram (ProgramS (definition:definitions)) = do
    location <- alloc
    let (FnDef _ ident _ _) = definition in modifyEnvironment (updateEnvironment ident location)
    updateStore location (Right definition)
    executeProgram (ProgramS definitions)

-------------------------------------------------------------------------------------------
-- Interpretery funkcji.
-------------------------------------------------------------------------------------------

executeFunction :: Ident -> [Expr] -> InterpretMonad Value

-- Funkcja interpretująca wykonanie funkcji. 
executeFunction ident exprs = do
    case ident of
        (Ident "printInt") -> case exprs of
            [] -> do
                return ValueVoid
            (e:ex) -> do
                val <- executeExpr e
                case val of 
                    (ValueInteger i) -> do
                        liftIO $ putStr $ (show i ++ " ")
                    _ -> throwError $ "Cannot printInt expression that is not of Int type."
                executeFunction (Ident "printInt") ex
        (Ident "printString") -> case exprs of
            [] -> do
                return ValueVoid
            (e:ex) -> do
                val <- executeExpr e
                case val of 
                    (ValueString s) -> do
                        liftIO $ putStr $ s
                        return ValueVoid
                    _ -> throwError $ "Cannot printString expression that is not of String type."
                executeFunction (Ident "printString") ex
        _ -> do
            object <- getObject ident
            case object of
                (Right (FnDef t (Ident s) args block)) -> do
                    env <- getEnvironment
                    
                    addArgs s args exprs
                    executeBlock block
                    releaseArgs args

                    putEnvironment env
                    case t of
                        -- TODO of course its a mock should return exact value
                        Int -> return (ValueInteger 0)
                        Str -> return (ValueString "")
                        Bool -> return (ValueBool False)
                        Void -> return ValueVoid
                -- _ -> let (Ident i) = ident in throwError $ "Function `" ++ i ++ "` was not declared in this scope."

data FunArgVal = FunArgValue Value | FunArgLocation Location

addArgs :: String -> [Arg] -> [Expr] -> InterpretMonad ()
addArgs s args exprs = do
    parsedArgs <- parseArgs s args exprs
    updateArgs args parsedArgs
    return ()

parseArgs :: String -> [Arg] -> [Expr] -> InterpretMonad [FunArgVal]
parseArgs s [] [] = do
    return []

parseArgs s (arg:args) (expr:exprs) = do
    funArgVal <- parseArg s arg expr
    funArgVals <- parseArgs s args exprs
    return (funArgVal:funArgVals)

parseArgs s (arg:args) _ = do
    throwError $ "Too few arguments of function " ++ s ++ "."

parseArgs s _ (expr:exprs) = do
    throwError $ "Too many arguments of function " ++ s ++ "."

parseArg :: String -> Arg -> Expr -> InterpretMonad FunArgVal
parseArg s (ValueArg t ident) expr = do
    value <- executeExpr expr
    case (t, value) of
        (Int, (ValueInteger _)) -> return $ FunArgValue value
        (Bool, (ValueBool _)) -> return $ FunArgValue value
        (Str, (ValueString _)) -> return $ FunArgValue value
        (Void, ValueVoid) -> return $ FunArgValue value
        _ -> let (Ident i) = ident in throwError $ "Invalid type of one argument passed by value of function `" ++ s ++ "`."

parseArg s (RefArg t ident) expr = do 
    case expr of 
        (EVar passedIdent) -> do
            value <- executeExpr expr
            case (t, value) of
                -- very ugly mock for type of argument
                (Int, (ValueInteger _)) -> do
                    location <- getLocation passedIdent
                    return $ FunArgLocation location
                (Bool, (ValueBool _)) -> do
                    location <- getLocation passedIdent
                    return $ FunArgLocation location
                (Str, (ValueString _)) -> do
                    location <- getLocation passedIdent
                    return $ FunArgLocation location
                (Void, ValueVoid) -> do
                    location <- getLocation passedIdent
                    return $ FunArgLocation location
                _ -> throwError $ "Invalid type of one argument passed by reference of function `" ++ s ++ "`."
        _ -> throwError $ "An argument of function `" ++ s ++ "` passed by reference is not an identifier."

updateArgs :: [Arg] -> [FunArgVal] -> InterpretMonad ()
updateArgs [] [] = do
    return ()

updateArgs (arg:args) (funArgVal:funArgVals) = do
    updateArg arg funArgVal
    updateArgs args funArgVals
    return ()

updateArg :: Arg -> FunArgVal -> InterpretMonad ()
updateArg (ValueArg t ident) (FunArgValue value) = do
    location <- alloc
    modifyEnvironment (updateEnvironment ident location)
    updateStore location (Left value)

updateArg (RefArg t ident) (FunArgLocation location) = do
    modifyEnvironment (updateEnvironment ident location)

releaseArgs :: [Arg] -> InterpretMonad ()
releaseArgs [] = do
    return ()

releaseArgs (arg:args) = do
    releaseArg arg
    releaseArgs args

releaseArg :: Arg -> InterpretMonad ()
releaseArg (ValueArg t ident) = do
    location <- getLocation ident
    releaseLocation location

releaseArg (RefArg t ident) = do 
    return ()


-------------------------------------------------------------------------------------------
-- Interpretery bloku.
-------------------------------------------------------------------------------------------

executeBlock :: Block -> InterpretMonad ()

-- Funkcja interpretująca wykonanie bloku.
executeBlock (BlockS stmts) = do
    env <- getEnvironment
    executeStmts stmts
    blockEnv <- getEnvironment
    let diff = Map.difference blockEnv env in 
        releaseLocations $ Map.elems diff
    putEnvironment env

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
executeStmt (BStmt b) = do
    executeBlock b

-- Funkcja interpretująca wykonanie statementu deklaracji.
executeStmt (Decl t declarations) = do
    executeStmtDecls t declarations

-- Funkcja interpretująca wykonanie statementu przypisania.
executeStmt (Ass ident exp) = do
    location <- getLocation ident
    object <- getObject ident
    newValue <- executeExpr exp
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

executeStmt (Cond expr stmt) = do
    val <- executeExpr expr
    case val of
        (ValueBool True) -> executeBlock (BlockS [stmt])
        (ValueBool False) -> return ()
        _ -> throwError $ "If expression is not of Bool type."

executeStmt (CondElse expr stmt1 stmt2) = do
    val <- executeExpr expr
    case val of
        (ValueBool True) -> executeBlock (BlockS [stmt1])
        (ValueBool False) -> executeBlock (BlockS [stmt2])
        _ -> throwError $ "If expression is not of Bool type."

executeStmt (While expr stmt) = do
    val <- executeExpr expr
    case val of
        (ValueBool True) -> do
            executeBlock (BlockS [stmt])
            executeStmt (While expr stmt)
        (ValueBool False) -> return ()
        _ -> throwError $ "While expression is not of Bool type."

executeStmt (SExp expr) = do
    executeExpr expr
    return ()

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
        Str -> updateStore location (Left (ValueString ""))
        Void -> updateStore location (Left ValueVoid)
        _ -> throwError $ "Unknown type."

-- Funkcja interpretująca deklarację obiektu z inicjalizacją.
executeStmtDecl t (Init ident expr) = do
    location <- alloc
    modifyEnvironment (updateEnvironment ident location)
    value <- executeExpr expr
    case (t, value) of
        (Int, (ValueInteger _)) -> updateStore location (Left value)
        (Bool, (ValueBool _)) -> updateStore location (Left value)
        (Str, (ValueString _)) -> updateStore location (Left value)
        (Void, ValueVoid) -> updateStore location (Left value)
        _ -> let (Ident i) = ident in throwError $ "Cannot assign to `" ++ i ++ "` an expression which is not of " ++ show t ++ " type."


-------------------------------------------------------------------------------------------
-- Interpretery wyrażeń.
-------------------------------------------------------------------------------------------

executeExpr :: Expr -> InterpretMonad Value

-- Funkcja interpretująca wartość liczby całkowitej.
executeExpr (ELitInt value) = return $ ValueInteger value

-- Funkcja interpretująca wartość zmiennej.
executeExpr (EVar ident) = do
    value <- getObject ident
    case value of
        (Left v) -> return $ v
        -- _ -> let (Ident i) = ident in throwError $ "`" ++ i ++ "` was not declared in this scope."

-- Funkcja interpretująca sumę dwóch wyrażeń.
executeExpr (EAdd exp1 op exp2) = do
    val1 <- executeExpr exp1
    val2 <- executeExpr exp2
    case (val1, val2) of
        (ValueInteger i1, ValueInteger i2) -> case op of
            Plus -> return $ ValueInteger (i1 + i2)
            Minus -> return $ ValueInteger (i1 - i2)
        _ -> throwError $ "Cannot apply any AddOperation to expressions of different types."

-- Funkcja interpretująca iloczyn dwóch wyrażeń.
executeExpr (EMul exp1 op exp2) = do
    val1 <- executeExpr exp1
    val2 <- executeExpr exp2
    case (val1, val2) of
        (ValueInteger i1, ValueInteger i2) -> case op of
            Times -> return $ ValueInteger (i1 * i2)
            Div -> return $ ValueInteger (i1 `div` i2)
            Mod -> return $ ValueInteger (i1 `mod` i2)
        _ -> throwError $ "Cannot apply any MulOperation to expressions of different types."

-- Funkcja interpretująca porównanie dwóch wyrażeń.
executeExpr (ERel exp1 op exp2) = do
    val1 <- executeExpr exp1
    val2 <- executeExpr exp2
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
executeExpr (EAnd exp1 exp2) = do
    val1 <- executeExpr exp1
    val2 <- executeExpr exp2
    case (val1, val2) of
        (ValueBool i1, ValueBool i2) -> return $ ValueBool (i1 && i2)
        _ -> throwError $ "Cannot apply RelOperation to expressions of different types."

-- Funkcja interpretująca alternatywę logiczną dwóch wyrażeń.
executeExpr (EOr exp1 exp2) = do
    val1 <- executeExpr exp1
    val2 <- executeExpr exp2
    case (val1, val2) of
        (ValueBool i1, ValueBool i2) -> return $ ValueBool (i1 || i2)
        _ -> throwError $ "Cannot apply RelOperation to expressions of different types."

-- Funkcja interpretująca wyrażenie przeciwne.
executeExpr (Neg exp) = do
    val <- executeExpr exp
    case val of
        ValueInteger i -> return $ ValueInteger (negate i)
        _ -> throwError $ "Cannot apply NegOperation to expression which is not of Int type."

-- Funkcja interpretująca negację wyrażenia.
executeExpr (Not exp) = do
    val <- executeExpr exp
    case val of
        ValueBool i -> return $ ValueBool (not i)
        _ -> throwError $ "Cannot apply NotOperation to expression which is not of Bool type."
    
-- Funkcja interpretująca literał "prawda".
executeExpr (ELitTrue) = do
    return $ ValueBool True

-- Funkcja interpretująca literał "fałsz".
executeExpr (ELitFalse) = do
    return $ ValueBool False

executeExpr (EApp i exprs) = do
    executeFunction i exprs

executeExpr (EString s) = do
    return $ ValueString s