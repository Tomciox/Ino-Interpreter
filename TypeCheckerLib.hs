-------------------------------
-- Autor:       Tomasz Madej --
-- Nr. albumu   385853       --
-------------------------------

{-# LANGUAGE MultiParamTypeClasses, NamedFieldPuns#-}

module TypeCheckerLib where 

import TypeCheckerStateLib
import TupleHelper

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

-------------------------------------------------------------------------------------------
-- Sprawdzanie poprawności typów programu.
-------------------------------------------------------------------------------------------

typeCheckProgram :: Program -> TypeCheckMonad ()

typeCheckProgram (ProgramS []) =
    return ()

typeCheckProgram (ProgramS (definition:definitions)) = do
    typeCheckStmt (FunDecl definition)
    typeCheckProgram (ProgramS definitions)

-------------------------------------------------------------------------------------------
-- Sprawdzenie poprawności typów w bloku.
-------------------------------------------------------------------------------------------

typeCheckBlock :: Block -> TypeCheckMonad ()

typeCheckBlock (BlockS stmts) = do
    backupEnvironment <- getEnvironment
    depth <- getDepth

    putDepth (depth + 1)
    
    typeCheckStmts stmts

    putEnvironment backupEnvironment
    putDepth depth

-------------------------------------------------------------------------------------------
-- Sprawdzanie poprawności typów statementów.
-------------------------------------------------------------------------------------------

typeCheckStmts :: [Stmt] -> TypeCheckMonad ()

typeCheckStmts [] = 
    return ()

typeCheckStmts (stmt:stmts) = do
    typeCheckStmt stmt
    typeCheckStmts stmts

typeCheckStmt :: Stmt -> TypeCheckMonad ()

typeCheckStmt Empty =
    return ()

typeCheckStmt Break = 
    return ()

typeCheckStmt Continue = 
    return ()

typeCheckStmt (BStmt b) =
    typeCheckBlock b

typeCheckStmt (Decl t declarations) =
    typeCheckStmtDecls t declarations

typeCheckStmt (FunDecl definition) = 
    let (FnDef t (Ident i) args (BlockS stmts)) = definition
        cast = (\x -> case x of
                (ValueArg t _) -> t
                (RefArg t _) -> t) in do
        depth <- getDepth
        putFunInfo (Ident i) t (map cast args)

        funName <- getFunName
        backupReturnType <- getReturnType
        depth <- getDepth
        environment <- getEnvironment

        putFunName i
        putReturnType (Return t)
        putDepth (depth + 1)

        typeCheckParseArgs args

        typeCheckStmts stmts

        putFunName funName
        putEnvironment environment
        putDepth depth
        putReturnType backupReturnType

typeCheckStmt (Ass ident exp) = do
    info <- getIdentInfo ident
    t <- typeCheckExpr exp
    case info of 
        (VarInfo t2 _) -> case (t == t2) of
            True -> return ()
            False -> let (Ident i) = ident in throwError $ "Ino TypeChecker Exception: Cannot assign to `" ++ i ++ "` which is of " ++ show t2 ++ " type, an expression of " ++ show t ++ " type."
        _ -> let (Ident i) = ident in throwError $ "Ino TypeChecker Exception: Cannot assign to `" ++ i ++ "` which is not a variable."

typeCheckStmt (AssTuple indices ident expr) = do
    info <- getIdentInfo ident
    t <- typeCheckExpr expr
    case info of
        (VarInfo tuple _ ) -> typeCheckAssignTuple tuple indices t
        _ -> let (Ident i) = ident in throwError $ "Ino TypeChecker Exception: `" ++ i ++ "` is not a tuple."

typeCheckStmt (Incr ident) = do
    info <- getIdentInfo ident
    case info of 
        (VarInfo t _) -> case t of
            Int -> return ()
            _ -> let (Ident i) = ident in throwError $ "Ino TypeChecker Exception: Cannot increment `" ++ i ++ "` which is of " ++ show t ++ " type."
        _ -> let (Ident i) = ident in throwError $ "Ino TypeChecker Exception: Cannot assign to `" ++ i ++ "` which is not a variable."

typeCheckStmt (Decr ident) = do
    info <- getIdentInfo ident
    case info of 
        (VarInfo t _) -> case t of
            Int -> return ()
            _ -> let (Ident i) = ident in throwError $ "Ino TypeChecker Exception: Cannot increment `" ++ i ++ "` which is of " ++ show t ++ " type."
        _ -> let (Ident i) = ident in throwError $ "Ino TypeChecker Exception: Cannot assign to `" ++ i ++ "` which is not a variable."

typeCheckStmt (Ret expr) = do
    (Return t) <- getReturnType
    t2 <- typeCheckExpr expr
    case (t == t2) of
        True -> return ()
        False -> do
            funName <- getFunName
            throwError $ "Ino TypeChecker Exception: return value of function `" ++ funName ++ "` is " ++ show t2 ++ " type, instead of " ++ show t ++ " type."

typeCheckStmt VRet = do
    returnType <- getReturnType
    case returnType of
        (Return Void) -> return ()
        _ -> do
            funName <- getFunName
            throwError $ "Ino TypeChecker Exception: return value of function `" ++ funName ++ "` cannot be " ++ show Void ++ " type."

typeCheckStmt (Cond expr stmt) = do
    t <- typeCheckExpr expr
    case t of
        Bool -> typeCheckBlock (BlockS [stmt])
        _ -> throwError $ "Ino TypeChecker Exception: If expression is not of Bool type."

typeCheckStmt (CondElse expr stmt1 stmt2) = do
    t <- typeCheckExpr expr
    case t of
        Bool -> do
            typeCheckBlock (BlockS [stmt1])
            typeCheckBlock (BlockS [stmt2])
        _ -> throwError $ "Ino TypeChecker Exception: If expression is not of Bool type."

typeCheckStmt (While expr stmt) = do
    t <- typeCheckExpr expr
    case t of
        Bool -> typeCheckBlock (BlockS [stmt])
        _ -> throwError $ "Ino TypeChecker Exception: While expression is not of Bool type."

typeCheckStmt (SExp expr) = do
    typeCheckExpr expr
    return ()

-------------------------------------------------------------------------------------------
-- Funkcje pomocnicze dla sprawdzania typów parsowania argumentów funkcji.
-------------------------------------------------------------------------------------------

typeCheckParseArgs :: [Arg] -> TypeCheckMonad ()
typeCheckParseArgs [] =
    return ()

typeCheckParseArgs (arg:args) = do
    case arg of 
        (ValueArg t ident) -> putVarInfo ident t
        (RefArg t ident) -> putVarInfo ident t
    typeCheckParseArgs args

-------------------------------------------------------------------------------------------
-- Funkcje pomocnicze dla sprawdzania typów statementów deklaracji zmiennych.
-------------------------------------------------------------------------------------------

typeCheckStmtDecls :: Type -> [Item] -> TypeCheckMonad ()

typeCheckStmtDecls t [] =
    return ()

typeCheckStmtDecls t (declaration:declarations) = do
    typeCheckStmtDecl t declaration
    typeCheckStmtDecls t declarations

typeCheckStmtDecl :: Type -> Item -> TypeCheckMonad ()

typeCheckStmtDecl t (NoInit ident) =
    putVarInfo ident t

typeCheckStmtDecl t (Init ident expr) = do
    t2 <- typeCheckExpr expr 
    case (t == t2) of
        True -> putVarInfo ident t
        False -> let (Ident i) = ident in throwError $ "Ino TypeChecker Exception: Cannot initialize `" ++ i ++ "` which is of " ++ show t ++ " type, with an expression of " ++ show t2 ++ " type."

-------------------------------------------------------------------------------------------
-- Wyliczanie typów wyrażeń.
-------------------------------------------------------------------------------------------

typeCheckExprs :: [Expr] -> TypeCheckMonad [Type]

typeCheckExprs [] =
    return []

typeCheckExprs (expr:exprs) = do
    value <- typeCheckExpr expr
    values <- typeCheckExprs exprs
    return $ value:values

typeCheckExpr :: Expr -> TypeCheckMonad Type

typeCheckExpr (EVar ident) = do
    info <- getIdentInfo ident
    case info of 
        (VarInfo t _) -> return t
        _ -> throwError $ "Ino TypeChecker Exception: Function identifier is not an expression."

typeCheckExpr (ETupleSubs indices ident) = do
    info <- getIdentInfo ident
    case info of
        (VarInfo tuple _) ->
            typeCheckGetTuple tuple indices
        _ -> let (Ident i) = ident in throwError $ "Ino TypeChecker Exception: `" ++ i ++ "` is not a tuple."

typeCheckExpr (EMakeTuple exprs) = do
    ts <- typeCheckExprs exprs
    return $ Tuple ts

typeCheckExpr (ELitInt _) =
    return Int

typeCheckExpr (ELitTrue) =
    return Bool

typeCheckExpr (ELitFalse) = 
    return Bool

typeCheckExpr (EApp ident exprs) =
    case ident of
        (Ident "print") -> return Void
        _ -> do
            identInfo <- getIdentInfo ident
            let (Ident i) = ident in case identInfo of
                (VarInfo _ _) -> throwError $ "Ino TypeChecker Exception: `" ++ i ++ "` is not a function." 
                (FunInfo t ts _) -> do
                    typeCheckParseArgsApp i exprs ts
                    return t

typeCheckExpr (EString _) =
    return Str

typeCheckExpr (Neg exp) = do
    t <- typeCheckExpr exp
    case t of
        Int -> return Int
        _ -> throwError $ "Ino TypeChecker Exception: Cannot apply NEG to expression which is not of Int type."

typeCheckExpr (Not exp) = do
    t <- typeCheckExpr exp
    case t of
        Bool -> return Bool
        _ -> throwError $ "Ino TypeChecker Exception: Cannot apply NOT to expression which is not of Bool type."

typeCheckExpr (EMul exp1 op exp2) = do
    t1 <- typeCheckExpr exp1
    t2 <- typeCheckExpr exp2
    case (t1, t2) of
        (Int, Int) -> return Int
        _ -> throwError $ "Ino TypeChecker Exception: Cannot apply any MUL operation on expressions of different types than Int."

typeCheckExpr (EAdd exp1 _ exp2) = do
    t1 <- typeCheckExpr exp1
    t2 <- typeCheckExpr exp2
    case (t1, t2) of
        (Int, Int) -> return Int
        _ -> throwError $ "Ino TypeChecker Exception: Cannot apply any ADD operation on expressions of different types than Int."

typeCheckExpr (ERel exp1 op exp2) = do
    t1 <- typeCheckExpr exp1
    t2 <- typeCheckExpr exp2
    case (t1, t2) of
        (Int, Int) -> return Bool
        _ -> throwError $ "Ino TypeChecker Exception: Cannot apply any RELATION operation on expressions of different types than Int."

typeCheckExpr (EAnd exp1 exp2) = do
    t1 <- typeCheckExpr exp1
    t2 <- typeCheckExpr exp2
    case (t1, t2) of
        (Bool, Bool) -> return Int
        _ -> throwError $ "Ino TypeChecker Exception: Cannot apply AND operation to expressions of different types than Bool."

typeCheckExpr (EOr exp1 exp2) = do
    t1 <- typeCheckExpr exp1
    t2 <- typeCheckExpr exp2
    case (t1, t2) of
        (Bool, Bool) -> return Int
        _ -> throwError $ "Ino TypeChecker Exception: Cannot apply OR operation to expressions of different types than Bool."

-------------------------------------------------------------------------------------------
-- Funkcje pomocnicze do aplikacji.
-------------------------------------------------------------------------------------------

typeCheckParseArgsApp :: String -> [Expr] -> [Type] -> TypeCheckMonad ()
typeCheckParseArgsApp _ [] [] =
    return ()

typeCheckParseArgsApp s (expr:exprs) (t:ts) = do
    t2 <- typeCheckExpr expr
    case (t == t2) of
        True -> return ()
        False -> throwError $ "Ino TypeChecker Exception: Invalid type of one argument, of function `" ++ s ++ "`."
    typeCheckParseArgsApp s exprs ts

typeCheckParseArgsApp s [] ts =
    throwError $ "Ino TypeChecker Exception: Too few arguments of function " ++ s ++ "."

typeCheckParseArgsApp s exprs [] =
    throwError $ "Ino TypeChecker Exception: Too many arguments of function " ++ s ++ "."