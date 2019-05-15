-------------------------------
-- Autor:       Tomasz Madej --
-- Nr. albumu   385853       --
-------------------------------

{-# LANGUAGE MultiParamTypeClasses, NamedFieldPuns#-}

module ExecuteLib where 

import ProgramStateLib
import TupleHelper
import PrintHelper

import LexIno
import ParIno
import SkelIno
import PrintIno
import AbsIno

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Identity

-------------------------------------------------------------------------------------------
-- Interpreter programu.
-------------------------------------------------------------------------------------------

executeProgram :: Program -> InterpretMonad ()

executeProgram (ProgramS []) = do
    object <- getObject (Ident "main")
    case object of
        (ObjectFunDef (FnDef Int _ [] _) environment) -> do
            resultReturn <- executeFunction (Ident "main") []
            case resultReturn of
                ResultUnit -> throwError $ "Ino Exception: Main ended without return statement."
                ResultBreak -> throwError $ "Ino Exception: Break statement without loop."
                ResultContinue -> throwError $ "Ino Exception: Continue statement without loop."
                ResultValue (ValueInteger 0) -> return ()
                ResultValue (ValueInteger r) -> throwError $ "Ino Exception: Main failed with exit code " ++ show r ++ "."
                _ -> throwError $ "Ino Exception: Main return value is not of Int type."
        _ -> throwError $ "Ino Exception: Main function is not defined."

executeProgram (ProgramS (definition:definitions)) = do
    executeStmt (FunDecl definition)
    executeProgram (ProgramS definitions)

-------------------------------------------------------------------------------------------
-- Interpreter wywołania funkcji.
-------------------------------------------------------------------------------------------

executeFunction :: Ident -> [Expr] -> InterpretMonad ResultReturn

executeFunction ident exprs = do
    case ident of
        (Ident "print") -> case exprs of
            [] -> do
                return $ ResultValue ValueVoid
            (e:ex) -> do
                value <- executeExpr e 
                printValue value 
                executeFunction (Ident "print") ex
        _ -> do
            object <- getObject ident
            case object of
                (ObjectFunDef (FnDef t (Ident s) args (BlockS stmts)) functionEnvironment) -> do
                    -- Zapisanie aktualnego środowiska.
                    backupEnvironment <- getEnvironment
                    -- Zapisanie aktualnej głębokości.
                    backupDepth <- getDepth
                    -- Sparsowanie argumentów, w środowisku wywołania.
                    parsedArgs <- parseArgs s args exprs
                    -- Przejście do środowiska z miejsca deklaracji funkcji (statyczna widoczność identyfikatorów).
                    putEnvironment functionEnvironment
                    -- Przejście do bloku wywołania funkcji.
                    putDepth (backupDepth + 1)
                    -- Dodanie deklaracji wywoływanej funkcji do środowiska jej wywołania, umożliwienie rekurencyjnego wywołania.
                    declareObject ident t object
                    -- Zdeklarowanie argumentów w bloku wywołania funkcji.
                    updateArgs args parsedArgs
                    -- Wywołanie funkcji i zwolnienie zaalokowanej przez nią pamięci.
                    environmentX <- getEnvironment
                    resultReturn <- executeStmts stmts
                    environmentY <- getEnvironment
                    releaseDifference environmentX environmentY
                    -- Zwolnienie pamięci zajmowanej przez argumenty wywołania funkcji.
                    releaseArgs args
                    -- Zwolnienie pamięci zajmowanej przez definicję funkcji dodaną dla umożliwienia rekurencyjnego wywołania.
                    (Info location _ _) <- getIdentInfo ident
                    releaseLocation location
                    -- Przywrócenie oryginalnego środowiska.
                    putEnvironment backupEnvironment
                    -- Przywrócenie oryginalnej głębokości.
                    putDepth backupDepth

                    case resultReturn of
                        ResultUnit -> throwError $ "Ino Exception: Function `" ++ s ++ "` ended without return statement."
                        ResultBreak -> throwError $ "Ino Exception: Break statement without loop."
                        ResultContinue -> throwError $ "Ino Exception: Continue statement without loop."
                        ResultValue value -> case (t == getValueType value) of
                            True -> return resultReturn
                            False -> throwError $ "Ino Exception: `" ++ s ++ "` return value is not of " ++ show t ++ " type."
                _ -> let (Ident i) = ident in throwError $ "Ino Exception: `" ++ i ++ "` is not a function."
    
-------------------------------------------------------------------------------------------
-- Funkcje pomocnicze dla `executeFunction`.
-------------------------------------------------------------------------------------------

-- Wyliczenie wartości argumentów, przed wywołaniem funkcji.
parseArgs :: String -> [Arg] -> [Expr] -> InterpretMonad [FunArgVal]

parseArgs s [] [] = do
    return []

parseArgs s (arg:args) (expr:exprs) = do
    funArgVal <- parseArg s arg expr
    funArgVals <- parseArgs s args exprs
    return (funArgVal:funArgVals)

parseArgs s (arg:args) _ = do
    throwError $ "Ino Exception: Too few arguments of function " ++ s ++ "."

parseArgs s _ (expr:exprs) = do
    throwError $ "Ino Exception: Too many arguments of function " ++ s ++ "."

-- Wyliczenie wartości jednego argumentu, przed uruchomieniem funkcji, w zależności od tego czy jest on przekazany przez wartość, czy referencję.
parseArg :: String -> Arg -> Expr -> InterpretMonad FunArgVal

parseArg s (ValueArg t ident) expr = do
    value <- executeExpr expr
    let exprT = getValueType value in case (t == exprT) of
        True -> return $ FunArgValue value
        _ -> let (Ident i) = ident in throwError $ "Ino Exception: Invalid type of one argument passed by value, of function `" ++ s ++ "`."

parseArg s (RefArg t ident) expr = do 
    case expr of 
        (EVar passedIdent) -> do
            (Info location passedIdentT _) <- getIdentInfo passedIdent
            case (t == passedIdentT) of
                True -> do
                    return $ FunArgLocation location
                _ -> throwError $ "Ino Exception: Invalid type of one argument passed by reference, of function `" ++ s ++ "`."
        _ -> throwError $ "Ino Exception: An argument of function `" ++ s ++ "` passed by reference is not an identifier."

-- Dodanie argumentów funkcji przed jej wywołaniem do środowiska/pamięci w którym będzie działać.
updateArgs :: [Arg] -> [FunArgVal] -> InterpretMonad ()

updateArgs [] [] = do
    return ()

updateArgs (arg:args) (funArgVal:funArgVals) = do
    updateArg arg funArgVal
    updateArgs args funArgVals

-- Dodanie jednego argumentu funkcji przed jej uruchomieniem do środowiska/pamięci w którym będzie działać.
updateArg :: Arg -> FunArgVal -> InterpretMonad ()

updateArg (ValueArg t ident) (FunArgValue value) = do
    declareObject ident t (ObjectValue value)

updateArg (RefArg t ident) (FunArgLocation location) = do
    depth <- getDepth
    updateEnvironment ident (Info location t depth)

-- Zwolnienie argumentów funkcji po jej uruchomieniu ze środowiska/pamięci w którym działała.
releaseArgs :: [Arg] -> InterpretMonad ()

releaseArgs [] = do
    return ()

releaseArgs (arg:args) = do
    releaseArg arg
    releaseArgs args

-- Zwolnienie jednego argumentu funkcji po jej uruchomieniu ze środowiska/pamięci w którym działała.
releaseArg :: Arg -> InterpretMonad ()

releaseArg (ValueArg _ ident) = do
    (Info location _ _) <- getIdentInfo ident
    releaseLocation location

releaseArg (RefArg t ident) = do 
    return ()

-------------------------------------------------------------------------------------------
-- Interpreter wywołania bloku.
-------------------------------------------------------------------------------------------

executeBlock :: Block -> InterpretMonad ResultReturn

executeBlock (BlockS stmts) = do
    -- Zapisanie aktualnego środowiska.
    backupEnvironment <- getEnvironment
    -- Zapisanie aktualnej głębokości.
    depth <- getDepth
    -- Przejście do bloku.
    putDepth (depth + 1)
    -- Wywołanie bloku.
    resultReturn <- executeStmts stmts
    -- Zwolnienie pamięci zaalokowanej w bloku.
    blockEnvironment <- getEnvironment
    releaseDifference backupEnvironment blockEnvironment 
    -- Przywrócenie oryginalnego środowiska.
    putEnvironment backupEnvironment
    -- Przywrócenie oryginalnej głębokości.
    putDepth depth

    return resultReturn

-------------------------------------------------------------------------------------------
-- Interpretery statementów.
-------------------------------------------------------------------------------------------

-- Funkcja interpretująca wykonanie listy statementów.
executeStmts :: [Stmt] -> InterpretMonad ResultReturn

executeStmts [] = do
    return ResultUnit

executeStmts (stmt:rest) = do
    resultReturn <- executeStmt stmt
    case resultReturn of
        ResultUnit -> executeStmts rest 
        _ -> return resultReturn

-- Funkcja interpretująca wykonanie pojedynczego statementu.
executeStmt :: Stmt -> InterpretMonad ResultReturn

executeStmt (Empty) = do
    return ResultUnit

executeStmt (BStmt b) = do
    executeBlock b

executeStmt (Decl t declarations) = do
    executeStmtDecls t declarations
    return ResultUnit

executeStmt (Ass ident exp) = do
    (Info location t _) <- getIdentInfo ident
    newValue <- executeExpr exp
    let newValueType = getValueType newValue in case (t == newValueType) of
        True -> updateStore location (ObjectValue newValue)
        _ -> let (Ident i) = ident in throwError $ "Ino Exception: Cannot assign to `" ++ i ++ "` which is of " ++ show t ++ " type, an expression of " ++ show newValueType ++ " type."
    return ResultUnit

executeStmt (Incr ident) = do
    (Info location t _) <- getIdentInfo ident
    case t of
        Int -> do
            (ObjectValue (ValueInteger value)) <- getObject ident
            updateStore location (ObjectValue (ValueInteger (value + 1)))
        _ -> let (Ident i) = ident in throwError $ "Ino Exception: Cannot increment `" ++ i ++ "` which is of " ++ show t ++ " type."
    return ResultUnit

executeStmt (Decr ident) = do
    (Info location t _) <- getIdentInfo ident
    case t of
        Int -> do
            (ObjectValue (ValueInteger value)) <- getObject ident
            updateStore location (ObjectValue (ValueInteger (value - 1)))
        _ -> let (Ident i) = ident in throwError $ "Ino Exception: Cannot decrement `" ++ i ++ "` which is of " ++ show t ++ " type."
    return ResultUnit

executeStmt (Cond expr stmt) = do
    val <- executeExpr expr
    case val of
        (ValueBool True) -> executeBlock (BlockS [stmt])
        (ValueBool False) -> return ResultUnit
        _ -> throwError $ "Ino Exception: If expression is not of Bool type."

executeStmt (CondElse expr stmt1 stmt2) = do
    val <- executeExpr expr
    case val of
        (ValueBool True) -> executeBlock (BlockS [stmt1])
        (ValueBool False) -> executeBlock (BlockS [stmt2])
        _ -> throwError $ "Ino Exception: If expression is not of Bool type."

executeStmt (While expr stmt) = do
    val <- executeExpr expr
    case val of
        (ValueBool True) -> do
            resultReturn <- executeBlock (BlockS [stmt])
            case resultReturn of
                ResultContinue -> executeStmt (While expr stmt)
                ResultUnit -> executeStmt (While expr stmt)
                ResultBreak -> return ResultUnit
                _ -> return resultReturn 
        (ValueBool False) -> return ResultUnit
        _ -> throwError $ "Ino Exception: While expression is not of Bool type."

executeStmt (SExp expr) = do
    executeExpr expr
    return ResultUnit

executeStmt (FunDecl definition) = do
    environment <- getEnvironment
    let (FnDef t ident _ _) = definition in do
        declareObject ident t (ObjectFunDef definition environment)
        return ResultUnit

executeStmt Break = 
    return ResultBreak

executeStmt Continue = 
    return ResultContinue

executeStmt (Ret expr) = do
    value <- executeExpr expr
    return $ ResultValue value

executeStmt VRet = 
    return $ ResultValue (ValueVoid)

executeStmt (AssTuple indices ident expr) = do
    object <- getObject ident
    case object of
        (ObjectValue (ValueTuple tuple)) -> do
            newValue <- executeExpr expr

            modifiedTuple <- assignTuple (ValueTuple tuple) indices newValue

            (Info location t _) <- getIdentInfo ident
            updateStore location (ObjectValue modifiedTuple)
            return ResultUnit
        _ -> let (Ident i) = ident in throwError $ "Ino Exception: `" ++ i ++ "` is not a tuple."

-------------------------------------------------------------------------------------------
-- Funkcje pomocnicze dla statementów deklaracji zmiennych.
-------------------------------------------------------------------------------------------

-- Funkcja interpretująca deklarację listy zmiennych.
executeStmtDecls :: Type -> [Item] -> InterpretMonad ()

executeStmtDecls t [] = do
    return ()

executeStmtDecls t (declaration:declarations) = do
    executeStmtDecl t declaration
    executeStmtDecls t declarations

-- Funkcja interpretująca deklarację pojedynczej zmiennej.
executeStmtDecl :: Type -> Item -> InterpretMonad ()

executeStmtDecl t (NoInit ident) = do
    declareObject ident t (ObjectValue (getDefaultValue t))

executeStmtDecl t (Init ident expr) = do
    value <- executeExpr expr 
    let valueType = getValueType value in case (t == valueType) of
        True -> declareObject ident t (ObjectValue value)
        False -> let (Ident i) = ident in throwError $ "Ino Exception: Cannot initialize `" ++ i ++ "` which is of " ++ show t ++ " type, with an expression of " ++ show valueType ++ " type."

-------------------------------------------------------------------------------------------
-- Interpretery wyrażeń.
-------------------------------------------------------------------------------------------

-- Funkcja interpretująca wartość listy wyrażeń.
executeExprs :: [Expr] -> InterpretMonad [Value]

executeExprs [] = do
    return []

executeExprs (expr:exprs) = do
    value <- executeExpr expr
    values <- executeExprs exprs
    return $ value:values

-- Funkcja interpretująca wartość pojedynczego wyrażenia.
executeExpr :: Expr -> InterpretMonad Value

executeExpr (ELitInt value) = return $ ValueInteger value

executeExpr (EVar ident) = do
    value <- getObject ident
    case value of
        (ObjectValue v) -> return $ v
        _ -> throwError $ "Ino Exception: Function identifier is not an expression."

executeExpr (EAdd exp1 op exp2) = do
    val1 <- executeExpr exp1
    val2 <- executeExpr exp2
    case (val1, val2) of
        (ValueInteger i1, ValueInteger i2) -> case op of
            Plus -> return $ ValueInteger (i1 + i2)
            Minus -> return $ ValueInteger (i1 - i2)
        _ -> throwError $ "Ino Exception: Cannot apply any ADD operation on expressions of different types than Int."

executeExpr (EMul exp1 op exp2) = do
    val1 <- executeExpr exp1
    val2 <- executeExpr exp2
    case (val1, val2) of
        (ValueInteger i1, ValueInteger i2) -> case op of
            Times -> return $ ValueInteger (i1 * i2)
            Div -> case i2 of
                0 -> throwError $ "Ino Exception: Division by zero."
                _ -> return $ ValueInteger (i1 `div` i2)
            Mod -> case i2 of
                0 -> throwError $ "Ino Exception: Modulo by zero."
                _ -> return $ ValueInteger (i1 `mod` i2)
        _ -> throwError $ "Ino Exception: Cannot apply any MUL operation on expressions of different types than Int."

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
        _ -> throwError $ "Ino Exception: Cannot apply any RELATION operation on expressions of different types than Int."

executeExpr (EAnd exp1 exp2) = do
    val1 <- executeExpr exp1
    val2 <- executeExpr exp2
    case (val1, val2) of
        (ValueBool i1, ValueBool i2) -> return $ ValueBool (i1 && i2)
        _ -> throwError $ "Ino Exception: Cannot apply AND operation to expressions of different types than Bool."

executeExpr (EOr exp1 exp2) = do
    val1 <- executeExpr exp1
    val2 <- executeExpr exp2
    case (val1, val2) of
        (ValueBool i1, ValueBool i2) -> return $ ValueBool (i1 || i2)
        _ -> throwError $ "Ino Exception: Cannot apply OR operation to expressions of different types than Bool."

executeExpr (Neg exp) = do
    val <- executeExpr exp
    case val of
        ValueInteger i -> return $ ValueInteger (negate i)
        _ -> throwError $ "Ino Exception: Cannot apply NEG to expression which is not of Int type."

executeExpr (Not exp) = do
    val <- executeExpr exp
    case val of
        ValueBool i -> return $ ValueBool (not i)
        _ -> throwError $ "Ino Exception: Cannot apply NOT to expression which is not of Bool type."
    
executeExpr (ELitTrue) = do
    return $ ValueBool True

executeExpr (ELitFalse) = do
    return $ ValueBool False

executeExpr (EApp i exprs) = do
    resultReturn <- executeFunction i exprs
    case resultReturn of
        ResultUnit -> throwError $ "Ino Exception: Function ended without return statement."
        ResultBreak -> throwError $ "Ino Exception: Break statement without loop."
        ResultContinue -> throwError $ "Ino Exception: Continue statement without loop."
        ResultValue r -> return r

executeExpr (EString s) = do
    return $ ValueString s

executeExpr (EMakeTuple exprs) = do
    values <- executeExprs exprs
    return $ ValueTuple values

executeExpr (ETupleSubs indices ident) = do
    object <- getObject ident

    case object of
        (ObjectValue (ValueTuple tuple)) -> do
            subTuple <- getTuple (ValueTuple tuple) indices

            return subTuple
        _ -> let (Ident i) = ident in throwError $ "Ino Exception: `" ++ i ++ "` is not a tuple."
