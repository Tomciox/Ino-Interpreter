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

data ResultReturn = ResultUnit | ResultBreak | ResultContinue | ResultValue Value

-- Typ wartości argumentu w trakcie parsowania. Jeśli jest przez wartość, to argument jest wyliczany, jeśli przez referencję, przekazywana jest lokacja.
data FunArgVal = FunArgValue Value | FunArgLocation Location

-------------------------------------------------------------------------------------------
-- Interpretery programu.
-------------------------------------------------------------------------------------------

executeProgram :: Program -> InterpretMonad ()

-- Funkcja interpretująca wykonanie pustego programu.
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
                _ -> throwError $ "Ino Exception: Main exit code is not of Int type."
        _ -> throwError $ "Ino Exception: Main function is not defined."

-- Funkcja interpretująca wykonanie niepustego programu.
executeProgram (ProgramS (definition:definitions)) = do
    executeStmt (FunDecl definition)
    executeProgram (ProgramS definitions)

-------------------------------------------------------------------------------------------
-- Interpretery funkcji.
-------------------------------------------------------------------------------------------

executeFunction :: Ident -> [Expr] -> InterpretMonad ResultReturn

-- Interpretacja wywołania funkcji. 
executeFunction ident exprs = do
    case ident of
        (Ident "print") -> case exprs of
            [] -> do
                return $ ResultValue ValueVoid
            (e:ex) -> do
                value <- executeExpr e
                case value of 
                    (ValueInteger i) -> do
                        liftIO $ putStr $ show i
                    (ValueBool b) -> do
                        liftIO $ putStr $ show b
                    (ValueVoid) -> do
                        liftIO $ putStr "()"
                    (ValueString s) -> do
                        liftIO $ putStr s
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
                    executeStmts stmts
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

                    -- TODO mock default type
                    return $ ResultValue $ getDefaultValue t
                _ -> let (Ident i) = ident in throwError $ "Ino Exception: `" ++ i ++ "` is not a function."
    
parseArgs :: String -> [Arg] -> [Expr] -> InterpretMonad [FunArgVal]

-- Wyliczenie wartości argumentów, przed uruchomieniem funkcji, w zależności od tego czy są przekazane przez wartość, czy referencję.
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

parseArg :: String -> Arg -> Expr -> InterpretMonad FunArgVal

-- Wyliczenie wartości jednego argumentu, przed uruchomieniem funkcji, w zależności od tego czy jest on przekazany przez wartość, czy referencję.
parseArg s (ValueArg t ident) expr = do
    value <- executeExpr expr
    let exprT = getValueType value in case (t == exprT) of
        True -> return $ FunArgValue value
        _ -> let (Ident i) = ident in throwError $ "Ino Exception: Invalid type of one argument passed by value of function `" ++ s ++ "`."

parseArg s (RefArg t ident) expr = do 
    case expr of 
        (EVar passedIdent) -> do
            (Info location passedIdentT _) <- getIdentInfo passedIdent
            case (t == passedIdentT) of
                True -> do
                    return $ FunArgLocation location
                _ -> throwError $ "Ino Exception: Invalid type of one argument passed by reference of function `" ++ s ++ "`."
        _ -> throwError $ "Ino Exception: An argument of function `" ++ s ++ "` passed by reference is not an identifier."

updateArgs :: [Arg] -> [FunArgVal] -> InterpretMonad ()

-- Dodanie argumentów funkcji przed jej uruchomieniem do środowiska/pamięci w którym będzie działać.
updateArgs [] [] = do
    return ()

updateArgs (arg:args) (funArgVal:funArgVals) = do
    updateArg arg funArgVal
    updateArgs args funArgVals

updateArg :: Arg -> FunArgVal -> InterpretMonad ()

-- Dodanie argumentu funkcji przed jej uruchomieniem do środowiska/pamięci w którym będzie działać.
updateArg (ValueArg t ident) (FunArgValue value) = do
    declareObject ident t (ObjectValue value)

updateArg (RefArg t ident) (FunArgLocation location) = do
    depth <- getDepth
    modifyEnvironment (updateEnvironment ident (Info location t depth))

releaseArgs :: [Arg] -> InterpretMonad ()

-- Zwolnienie argumentów funkcji po jej uruchomieniu ze środowiska/pamięci w którym działała.
releaseArgs [] = do
    return ()

releaseArgs (arg:args) = do
    releaseArg arg
    releaseArgs args

releaseArg :: Arg -> InterpretMonad ()

-- Zwolnienie argumentu funkcji po jej uruchomieniu ze środowiska/pamięci w którym działała.
releaseArg (ValueArg _ ident) = do
    (Info location _ _) <- getIdentInfo ident
    releaseLocation location

releaseArg (RefArg t ident) = do 
    return ()

-------------------------------------------------------------------------------------------
-- Interpretery bloku.
-------------------------------------------------------------------------------------------


executeBlock :: Block -> InterpretMonad ()

-- Funkcja interpretująca wykonanie bloku.
executeBlock (BlockS stmts) = do
    -- Zapisanie aktualnego środowiska.
    backupEnvironment <- getEnvironment
    -- Zapisanie aktualnej głębokości.
    depth <- getDepth
    -- Przejście do bloku.
    putDepth (depth + 1)
    -- Wywołanie bloku.
    executeStmts stmts
    -- Zwolnienie pamięci zaalokowanej w bloku.
    blockEnvironment <- getEnvironment
    releaseDifference backupEnvironment blockEnvironment 
    -- Przywrócenie oryginalnego środowiska.
    putEnvironment backupEnvironment
    -- Przywrócenie oryginalnej głębokości.
    putDepth depth

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
    (Info location t _) <- getIdentInfo ident
    newValue <- executeExpr exp
    let newValueType = getValueType newValue in case (t == newValueType) of
        True -> updateStore location (ObjectValue newValue)
        _ -> let (Ident i) = ident in throwError $ "Ino Exception: Cannot assign to `" ++ i ++ "` which is of " ++ show t ++ " type, an expression of " ++ show newValueType ++ " type."

-- Funkcja interpretująca wykonanie statementu inkrementacji zmiennej.
executeStmt (Incr ident) = do
    (Info location t _) <- getIdentInfo ident
    case t of
        Int -> do
            (ObjectValue (ValueInteger value)) <- getObject ident
            updateStore location (ObjectValue (ValueInteger (value + 1)))
        _ -> let (Ident i) = ident in throwError $ "Ino Exception: Cannot increment `" ++ i ++ "` which is of " ++ show t ++ " type."

-- Funkcja interpretująca wykonanie statementu dekrementacji zmiennej.
executeStmt (Decr ident) = do
    (Info location t _) <- getIdentInfo ident
    case t of
        Int -> do
            (ObjectValue (ValueInteger value)) <- getObject ident
            updateStore location (ObjectValue (ValueInteger (value - 1)))
        _ -> let (Ident i) = ident in throwError $ "Ino Exception: Cannot decrement `" ++ i ++ "` which is of " ++ show t ++ " type."

-- Funkcja interpretująca wykonanie statementu warunku if bez else.
executeStmt (Cond expr stmt) = do
    val <- executeExpr expr
    case val of
        (ValueBool True) -> executeBlock (BlockS [stmt])
        (ValueBool False) -> return ()
        _ -> throwError $ "Ino Exception: If expression is not of Bool type."

-- Funkcja interpretująca wykonanie statementu warunku if z else.
executeStmt (CondElse expr stmt1 stmt2) = do
    val <- executeExpr expr
    case val of
        (ValueBool True) -> executeBlock (BlockS [stmt1])
        (ValueBool False) -> executeBlock (BlockS [stmt2])
        _ -> throwError $ "Ino Exception: If expression is not of Bool type."

-- Funkcja interpretująca wykonanie statementu pętli while.
executeStmt (While expr stmt) = do
    val <- executeExpr expr
    case val of
        (ValueBool True) -> do
            executeBlock (BlockS [stmt])
            executeStmt (While expr stmt)
        (ValueBool False) -> return ()
        _ -> throwError $ "Ino Exception: While expression is not of Bool type."

-- Funkcja interpretująca wykonanie statementu będącego wyrażniem.
executeStmt (SExp expr) = do
    -- TODO chceck this ???
    value <- executeExpr expr
    return ()

executeStmt (FunDecl definition) = do
    environment <- getEnvironment
    let (FnDef t ident _ _) = definition in 
        declareObject ident t (ObjectFunDef definition environment)

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
    declareObject ident t (ObjectValue (getDefaultValue t))

-- Funkcja interpretująca deklarację obiektu z inicjalizacją.
executeStmtDecl t (Init ident expr) = do
    value <- executeExpr expr 
    let valueType = getValueType value in case (t == valueType) of
        True -> declareObject ident t (ObjectValue value)
        False -> let (Ident i) = ident in throwError $ "Ino Exception: Cannot initialize `" ++ i ++ "` which is of " ++ show t ++ " type, with an expression of " ++ show valueType ++ " type."

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
        (ObjectValue v) -> return $ v
        _ -> throwError $ "Ino Exception: Function identifier is not an expression."

-- Funkcja interpretująca sumę dwóch wyrażeń.
executeExpr (EAdd exp1 op exp2) = do
    val1 <- executeExpr exp1
    val2 <- executeExpr exp2
    case (val1, val2) of
        (ValueInteger i1, ValueInteger i2) -> case op of
            Plus -> return $ ValueInteger (i1 + i2)
            Minus -> return $ ValueInteger (i1 - i2)
        _ -> throwError $ "Ino Exception: Cannot apply any ADD operation on expressions of different types than Int."

-- Funkcja interpretująca iloczyn dwóch wyrażeń.
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
        _ -> throwError $ "Ino Exception: Cannot apply any RELATION operation on expressions of different types than Int."

-- Funkcja interpretująca sumę logiczną dwóch wyrażeń.
executeExpr (EAnd exp1 exp2) = do
    val1 <- executeExpr exp1
    val2 <- executeExpr exp2
    case (val1, val2) of
        (ValueBool i1, ValueBool i2) -> return $ ValueBool (i1 && i2)
        _ -> throwError $ "Ino Exception: Cannot apply AND operation to expressions of different types than Bool."

-- Funkcja interpretująca alternatywę logiczną dwóch wyrażeń.
executeExpr (EOr exp1 exp2) = do
    val1 <- executeExpr exp1
    val2 <- executeExpr exp2
    case (val1, val2) of
        (ValueBool i1, ValueBool i2) -> return $ ValueBool (i1 || i2)
        _ -> throwError $ "Ino Exception: Cannot apply OR operation to expressions of different types than Bool."

-- Funkcja interpretująca wyrażenie przeciwne.
executeExpr (Neg exp) = do
    val <- executeExpr exp
    case val of
        ValueInteger i -> return $ ValueInteger (negate i)
        _ -> throwError $ "Ino Exception: Cannot apply NEG to expression which is not of Int type."

-- Funkcja interpretująca negację wyrażenia.
executeExpr (Not exp) = do
    val <- executeExpr exp
    case val of
        ValueBool i -> return $ ValueBool (not i)
        _ -> throwError $ "Ino Exception: Cannot apply NOT to expression which is not of Bool type."
    
-- Funkcja interpretująca literał "prawda".
executeExpr (ELitTrue) = do
    return $ ValueBool True

-- Funkcja interpretująca literał "fałsz".
executeExpr (ELitFalse) = do
    return $ ValueBool False

-- Funkcja interpretująca aplikację funkcji do wyrażeń.
executeExpr (EApp i exprs) = do
    resultReturn <- executeFunction i exprs
    case resultReturn of
        ResultUnit -> throwError $ "Ino Exception: Function ended without return statement."
        ResultBreak -> throwError $ "Ino Exception: Break statement without loop."
        ResultContinue -> throwError $ "Ino Exception: Continue statement without loop."
        ResultValue r -> return r

-- Funkcja interpretująca ciąg znaków.
executeExpr (EString s) = do
    return $ ValueString s