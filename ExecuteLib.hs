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
        (ObjectTopDef (FnDef Int _ [] _)) -> do
            retVal <- executeFunction (Ident "main") []
            case retVal of
                -- TODO
                (ValueInteger 0) -> return ()
                (ValueInteger r) -> throwError $ "Program failed with exit code " ++ show r ++ "."
        _ -> throwError $ "Main function is not defined."

-- Funkcja interpretująca wykonanie niepustego programu.
executeProgram (ProgramS (definition:definitions)) = do
    depth <- getDepth
    let (FnDef t ident _ _) = definition in putNew ident t depth (ObjectTopDef definition)
    executeProgram (ProgramS definitions)

-------------------------------------------------------------------------------------------
-- Interpretery funkcji.
-------------------------------------------------------------------------------------------

executeFunction :: Ident -> [Expr] -> InterpretMonad Value

-- Interpretacja wywołania funkcji. 
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
                (ObjectTopDef (FnDef t (Ident s) args (BlockS stmts))) -> do
                    env <- getEnvironment
                    depth <- getDepth
                    putDepth (depth + 1)
                    
                    addArgs s args exprs

                    executeStmts stmts

                    releaseArgs args

                    putEnvironment env
                    putDepth depth
                    case t of
                        -- TODO of course its a mock should return exact value
                        Int -> return (ValueInteger 0)
                        Str -> return (ValueString "")
                        Bool -> return (ValueBool False)
                        Void -> return ValueVoid
                _ -> let (Ident i) = ident in throwError $ "Function `" ++ i ++ "` was not declared in this scope."

-- Typ wartości argumentu w trakcie parsowania. Jeśli jest przez wartość, to argument jest wyliczany, jeśli przez referencję, przekazywana jest lokacja.
data FunArgVal = FunArgValue Value | FunArgLocation Location

addArgs :: String -> [Arg] -> [Expr] -> InterpretMonad ()

-- Wylicznie i dodanie argumentów funkcji do środowiska/pamięci, przed jej uruchomieniem.
addArgs s args exprs = do
    parsedArgs <- parseArgs s args exprs
    updateArgs args parsedArgs
    return ()

parseArgs :: String -> [Arg] -> [Expr] -> InterpretMonad [FunArgVal]

-- Wyliczenie wartości argumentów, przed uruchomieniem funkcji, w zależności od tego czy są przekazane przez wartość, czy referencję.
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

-- Wyliczenie wartości jednego argumentu, przed uruchomieniem funkcji, w zależności od tego czy jest on przekazany przez wartość, czy referencję.
parseArg s (ValueArg t ident) expr = do
    value <- executeExpr expr
    let exprT = getValueType value in case (t == exprT) of
        True -> return $ FunArgValue value
        _ -> let (Ident i) = ident in throwError $ "Invalid type of one argument passed by value of function `" ++ s ++ "`."

parseArg s (RefArg t ident) expr = do 
    case expr of 
        (EVar passedIdent) -> do
            (Info location passedIdentT _) <- getIdentInfo passedIdent
            case (t == passedIdentT) of
                True -> do
                    return $ FunArgLocation location
                _ -> throwError $ "Invalid type of one argument passed by reference of function `" ++ s ++ "`."
        _ -> throwError $ "An argument of function `" ++ s ++ "` passed by reference is not an identifier."

updateArgs :: [Arg] -> [FunArgVal] -> InterpretMonad ()

-- Dodanie argumentów funkcji przed jej uruchomieniem do środowiska/pamięci w którym będzie działać.
updateArgs [] [] = do
    return ()

updateArgs (arg:args) (funArgVal:funArgVals) = do
    updateArg arg funArgVal
    updateArgs args funArgVals
    return ()

updateArg :: Arg -> FunArgVal -> InterpretMonad ()

-- Dodanie argumentu funkcji przed jej uruchomieniem do środowiska/pamięci w którym będzie działać.
updateArg (ValueArg t ident) (FunArgValue value) = do
    location <- alloc
    depth <- getDepth
    modifyEnvironment (updateEnvironment ident (Info location t depth))
    updateStore location (ObjectValue value)

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
releaseArg (ValueArg t ident) = do
    (Info location t depth) <- getIdentInfo ident
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
    depth <- getDepth
    putDepth (depth + 1)

    executeStmts stmts

    blockEnv <- getEnvironment

    let diff = Map.differenceWith (\ x y -> if x == y then Nothing else (Just y)) blockEnv env in 
        releaseLocations $ map (\(Info l _ _) -> l) (Map.elems diff)

    putEnvironment env
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
        _ -> let (Ident i) = ident in throwError $ "Cannot assign to `" ++ i ++ "` which is of " ++ show t ++ " type, an expression of " ++ show newValueType ++ " type."

-- Funkcja interpretująca wykonanie statementu inkrementacji zmiennej.
executeStmt (Incr ident) = do
    (Info location t _) <- getIdentInfo ident
    case t of
        Int -> do
            (ObjectValue (ValueInteger value)) <- getObject ident
            updateStore location (ObjectValue (ValueInteger (value + 1)))
        _ -> let (Ident i) = ident in throwError $ "Cannot increment `" ++ i ++ "` which is of " ++ show t ++ " type."

-- Funkcja interpretująca wykonanie statementu dekrementacji zmiennej.
executeStmt (Decr ident) = do
    (Info location t _) <- getIdentInfo ident
    case t of
        Int -> do
            (ObjectValue (ValueInteger value)) <- getObject ident
            updateStore location (ObjectValue (ValueInteger (value - 1)))
        _ -> let (Ident i) = ident in throwError $ "Cannot decrement `" ++ i ++ "` which is of " ++ show t ++ " type."

-- Funkcja interpretująca wykonanie statementu warunku if bez else.
executeStmt (Cond expr stmt) = do
    val <- executeExpr expr
    case val of
        (ValueBool True) -> executeBlock (BlockS [stmt])
        (ValueBool False) -> return ()
        _ -> throwError $ "If expression is not of Bool type."

-- Funkcja interpretująca wykonanie statementu warunku if z else.
executeStmt (CondElse expr stmt1 stmt2) = do
    val <- executeExpr expr
    case val of
        (ValueBool True) -> executeBlock (BlockS [stmt1])
        (ValueBool False) -> executeBlock (BlockS [stmt2])
        _ -> throwError $ "If expression is not of Bool type."

-- Funkcja interpretująca wykonanie statementu pętli while.
executeStmt (While expr stmt) = do
    val <- executeExpr expr
    case val of
        (ValueBool True) -> do
            executeBlock (BlockS [stmt])
            executeStmt (While expr stmt)
        (ValueBool False) -> return ()
        _ -> throwError $ "While expression is not of Bool type."

-- Funkcja interpretująca wykonanie statementu będącego wyrażniem.
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
    executeStmtDeclHelper t ident (getDefaultValue t)

-- Funkcja interpretująca deklarację obiektu z inicjalizacją.
executeStmtDecl t (Init ident expr) = do
    value <- executeExpr expr 
    executeStmtDeclHelper t ident value

-- Funkcja pomocnicza dodająca do środowiska i pamięci nową zmienną o zadanym typie i wartości.
executeStmtDeclHelper :: Type -> Ident -> Value -> InterpretMonad ()
executeStmtDeclHelper t ident value = do
    let valueType = getValueType value in case (t == valueType) of
        True -> do
            maybeIdentInfo <- getMaybeIdentInfo ident
            actualDepth <- getDepth
            case maybeIdentInfo of
                (Just (Info location _ depth)) -> do 
                    case (depth < actualDepth) of
                        True -> putNew ident t actualDepth (ObjectValue value)
                        False -> let (Ident i) = ident in throwError $ "Redeclaration of `" ++ i ++ "`."
                Nothing -> do
                    putNew ident t actualDepth (ObjectValue value)
        False -> let (Ident i) = ident in throwError $ "Cannot initialize `" ++ i ++ "` which is of " ++ show t ++ " type, with an expression of " ++ show valueType ++ " type."
        
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

-- Funkcja interpretująca sumę dwóch wyrażeń.
executeExpr (EAdd exp1 op exp2) = do
    val1 <- executeExpr exp1
    val2 <- executeExpr exp2
    case (val1, val2) of
        (ValueInteger i1, ValueInteger i2) -> case op of
            Plus -> return $ ValueInteger (i1 + i2)
            Minus -> return $ ValueInteger (i1 - i2)
        _ -> throwError $ "Cannot apply any ADD operation on expressions of different types than Int."

-- Funkcja interpretująca iloczyn dwóch wyrażeń.
executeExpr (EMul exp1 op exp2) = do
    val1 <- executeExpr exp1
    val2 <- executeExpr exp2
    case (val1, val2) of
        (ValueInteger i1, ValueInteger i2) -> case op of
            Times -> return $ ValueInteger (i1 * i2)
            Div -> return $ ValueInteger (i1 `div` i2)
            Mod -> return $ ValueInteger (i1 `mod` i2)
        _ -> throwError $ "Cannot apply any MUL operation on expressions of different types than Int."

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
        _ -> throwError $ "Cannot apply any RELATION operation on expressions of different types than Int."

-- Funkcja interpretująca sumę logiczną dwóch wyrażeń.
executeExpr (EAnd exp1 exp2) = do
    val1 <- executeExpr exp1
    val2 <- executeExpr exp2
    case (val1, val2) of
        (ValueBool i1, ValueBool i2) -> return $ ValueBool (i1 && i2)
        _ -> throwError $ "Cannot apply AND operation to expressions of different types than Bool."

-- Funkcja interpretująca alternatywę logiczną dwóch wyrażeń.
executeExpr (EOr exp1 exp2) = do
    val1 <- executeExpr exp1
    val2 <- executeExpr exp2
    case (val1, val2) of
        (ValueBool i1, ValueBool i2) -> return $ ValueBool (i1 || i2)
        _ -> throwError $ "Cannot apply OR operation to expressions of different types than Bool."

-- Funkcja interpretująca wyrażenie przeciwne.
executeExpr (Neg exp) = do
    val <- executeExpr exp
    case val of
        ValueInteger i -> return $ ValueInteger (negate i)
        _ -> throwError $ "Cannot apply NEG to expression which is not of Int type."

-- Funkcja interpretująca negację wyrażenia.
executeExpr (Not exp) = do
    val <- executeExpr exp
    case val of
        ValueBool i -> return $ ValueBool (not i)
        _ -> throwError $ "Cannot apply NOT to expression which is not of Bool type."
    
-- Funkcja interpretująca literał "prawda".
executeExpr (ELitTrue) = do
    return $ ValueBool True

-- Funkcja interpretująca literał "fałsz".
executeExpr (ELitFalse) = do
    return $ ValueBool False

-- Funkcja interpretująca aplikację funkcji do wyrażeń.
executeExpr (EApp i exprs) = do
    executeFunction i exprs

-- Funkcja interpretująca ciąg znaków.
executeExpr (EString s) = do
    return $ ValueString s