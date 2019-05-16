-------------------------------
-- Autor:       Tomasz Madej --
-- Nr. albumu   385853       --
-------------------------------

{-# LANGUAGE MultiParamTypeClasses, NamedFieldPuns#-}

module TypeChecker where 

import TypeCheckStateLib

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
-- Sprawdzanie poprawności typów programu.
-------------------------------------------------------------------------------------------

typeCheckProgram :: Program -> TypeCheckMonad ()

typeCheckProgram (ProgramS []) = do
    return ()

typeCheckProgram (ProgramS (definition:definitions)) = do
    typeCheckStmt (FunDecl definition)
    typeCheckProgram (ProgramS definitions)

-------------------------------------------------------------------------------------------
-- Sprawdzanie poprawności typów wywołania funkcji.
-------------------------------------------------------------------------------------------

-------------------------------------------------------------------------------------------
-- Sprawdzanie poprawności typów wyrażenia.
-------------------------------------------------------------------------------------------

typeCheckStmts :: [Stmt] -> TypeCheckMonad ()
typeCheckStmts [] = 
    return ()

typeCheckStmts (stmt:stmts) = do
    typeCheckStmt stmt
    typeCheckStmts stmts

typeCheckStmt :: Stmt -> TypeCheckMonad ()
typeCheckStmt (FunDecl definition) = do
    let (FnDef t ident args (BlockS stmts)) = definition in do
        environment <- getEnvironment
        declareObject ident t (ObjectFunDef (FnDef t ident args (BlockS [])) environment)
        typeCheckStmts stmts

-- typeCheckStmt _ = return ()


