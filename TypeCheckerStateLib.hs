-------------------------------
-- Autor:       Tomasz Madej --
-- Nr. albumu   385853       --
-------------------------------

{-# LANGUAGE MultiParamTypeClasses, NamedFieldPuns#-}

module TypeCheckerStateLib where 

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
-- Typy używane do przechowywania stanu sprawdzania typów programu.
-------------------------------------------------------------------------------------------

-- Aktualnej głębokość drzewa sprawdzania typów.
type Depth = Int

-- Definicja początkowej głębokości drzewa sprawdzania typów.
initialDepth :: Depth
initialDepth = 0

-- Informacje o obiekcie przechowywane w środowisku.
-- Dla zmiennej, pamiętany jest jej typ oraz głębokość ostatniej deklaracji.
-- Dla funkcji, pamiętany jest typ przez nią zwracany, typy argumentów oraz głębokość ostatniej deklaracji.
data IdentInfo = VarInfo Type Depth | FunInfo Type [Type] Depth
    deriving(Eq, Ord, Show, Read)

-- Środowisko zmiennych i funkcji programu.
type Environment = Map.Map Ident IdentInfo

-- Definicja początkowo pustego środowiska programu.
initialEnvironment :: Environment
initialEnvironment = Map.empty

-- Służy do przechowywania typu zwracanego wyniku w aktualnym miejscu drzewa.
data ReturnType = ReturnUnit | Return Type

-- Aktualny stan sprawdzania typów programu.
data TypeCheckState = TypeCheckState { 
    funName :: String,
    returnType :: ReturnType,
    depth :: Depth,
    environment :: Environment }

-- Zwrócenie nazwy najgłębszej w drzewie funkcji w której się aktualnie znajdujemy, do wypisywania errorów.
getFunName :: TypeCheckMonad String
getFunName = gets funName

-- Ustalenie nazwy funkcji w której się aktualnie znajdujemy.
putFunName :: String -> TypeCheckMonad ()
putFunName funName = modify $ \state -> 
    state { funName }

-- Zwrócenie typu returnowanego wyniku najgłębszej w drzewie funkcji w której się aktualnie znajdujemy.
getReturnType :: TypeCheckMonad ReturnType
getReturnType = gets returnType

-- Ustalenie returnowanego wyniku funkcji w której się aktualnie znajdujemy.
putReturnType :: ReturnType -> TypeCheckMonad ()
putReturnType returnType = modify $ \state -> 
    state { returnType }

-- Zwrócenie aktualnej głębokości programu.
getDepth :: TypeCheckMonad Depth
getDepth = gets depth

-- Ustalenie aktualnej głębokości programu na zadaną argumentem.
putDepth :: Depth -> TypeCheckMonad ()
putDepth depth = modify $ \state -> 
    state { depth }

-- Zwrócenie aktualnego środowiska programu.
getEnvironment :: TypeCheckMonad Environment
getEnvironment = gets environment

-- Ustalenie aktualnego środowiska programu na zadane argumentem.
putEnvironment :: Environment -> TypeCheckMonad ()
putEnvironment environment = modify $ \state -> 
    state { environment }

-- Funkcja aktualizująca aktualne środowisko o nowy identyfikator.
updateEnvironment :: Ident -> IdentInfo -> TypeCheckMonad ()
updateEnvironment ident info = modify $ \state -> 
    state { environment = Map.insert ident info (environment state) }

-- Początkowy pusty stan sprawdzania typów prgramu.
initialState = TypeCheckState "" ReturnUnit initialDepth initialEnvironment

-------------------------------------------------------------------------------------------
-- Monada służąca do sprawdzania typów programu.
-------------------------------------------------------------------------------------------

type TypeCheckMonad a = StateT TypeCheckState (ExceptT String IO) a

runTypeCheckMonad :: TypeCheckMonad a -> TypeCheckState -> IO (Either String (a, TypeCheckState))
runTypeCheckMonad m state = runExceptT (runStateT m state)

-------------------------------------------------------------------------------------------
-- Funkcje pomocnicze do operacji na stanie sprawdzania typów programu.
-------------------------------------------------------------------------------------------

-- Zwrócenie informacji związanych z identyfikatorem, na które wskazuje zadany identyfikator.
getMaybeIdentInfo :: Ident -> TypeCheckMonad (Maybe IdentInfo)
getMaybeIdentInfo ident = do
    environment <- getEnvironment
    return $ Map.lookup ident environment

-- Zwrócenie informacji związanych z identyfikatorem, na które wskazuje zadany identyfikator.
getIdentInfo :: Ident -> TypeCheckMonad IdentInfo
getIdentInfo ident = do
    maybeIdentInfo <- getMaybeIdentInfo ident
    case maybeIdentInfo of
        Nothing -> let (Ident i) = ident in 
            throwError $ "Ino TypeChecker Exception: `" ++ i ++ "` was not declared in this scope."
        (Just identInfo) -> return identInfo

-- Dodanie nowej zmiennej do środowiska.
putVarInfo :: Ident -> Type -> TypeCheckMonad ()
putVarInfo ident t = do
    maybeIdentInfo <- getMaybeIdentInfo ident
    actualDepth <- getDepth
    case maybeIdentInfo of
        Nothing -> updateEnvironment ident (VarInfo t actualDepth)
        (Just (VarInfo _ declarationDepth)) -> case (actualDepth > declarationDepth) of
            True -> updateEnvironment ident (VarInfo t actualDepth)
            False -> let (Ident i) = ident in throwError $ "Ino TypeChecker Exception: Redeclaration of `" ++ i ++ "`."
        (Just (FunInfo _ _ declarationDepth)) -> case (actualDepth > declarationDepth) of
            True -> updateEnvironment ident (VarInfo t actualDepth)
            False -> let (Ident i) = ident in throwError $ "Ino TypeChecker Exception: Redeclaration of `" ++ i ++ "`."

-- Dodanie nowej funkcji do środowiska.
putFunInfo :: Ident -> Type -> [Type] -> TypeCheckMonad ()
putFunInfo ident t ts = do
    maybeIdentInfo <- getMaybeIdentInfo ident
    actualDepth <- getDepth
    case maybeIdentInfo of
        Nothing -> updateEnvironment ident (FunInfo t ts actualDepth)
        (Just (VarInfo _ declarationDepth)) -> case (actualDepth > declarationDepth) of
            True -> updateEnvironment ident (FunInfo t ts actualDepth)
            False -> let (Ident i) = ident in throwError $ "Ino TypeChecker Exception: Redeclaration of `" ++ i ++ "`."
        (Just (FunInfo _ _ declarationDepth)) -> case (actualDepth > declarationDepth) of
            True -> updateEnvironment ident (FunInfo t ts actualDepth)
            False -> let (Ident i) = ident in throwError $ "Ino TypeChecker Exception: Redeclaration of `" ++ i ++ "`."
    