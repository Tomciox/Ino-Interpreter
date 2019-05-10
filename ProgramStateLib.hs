-------------------------------
-- Autor:       Tomasz Madej --
-- Nr. albumu   385853       --
-------------------------------

{-# LANGUAGE MultiParamTypeClasses, NamedFieldPuns#-}

-- Moduł zawierający zawierający definicje oraz funkcje pomocnicze do operacji na aktualnym stanie programu.

module ProgramStateLib where 

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
-- Podstawowe definicje typów oraz ich początkowych konstruktorów i akcesorów.
-------------------------------------------------------------------------------------------

-- Typ danych wynikowych dla wyrażeń.
data Value = ValueInteger Integer | ValueBool Bool | ValueVoid | ValueString String 
    deriving(Eq, Ord, Show, Read)

-- TODO inne typy ???
-- Definicja typu przechowywanego w pamięci stanu programu.
type Object = Either Value TopDef

-- Definicja typu pamięci programu.
type Store = Map.Map Location Object

-- Definicja początkowej pustej pamięci programu.
initialStore :: Store
initialStore = Map.empty

-- Definicja typu lokację w pamięci stanu programu.
type Location = Int

-- Definicja początkowo wolnych lokacji.
initialFreeLocations :: [Location]
initialFreeLocations = [1..2^16]

-- Definicja typu środowiska programu.
type Environment = Map.Map Ident [Location]

-- Definicja początkowo pustego środowiska programu.
initialEnvironment :: Environment
initialEnvironment = Map.empty

-- Struktura przechowująca aktualny stan programu.
data ProgramState = ProgramState { 
    store :: Store, 
    freeLocations :: [Location], 
    environment :: Environment }

-- Funkcja zwracająca aktualną pamięć programu.
getStore :: InterpretMonad Store  
getStore = gets store

-- TODO lista lokacji ???
-- Funkcja zwracająca aktualne wolne lokacje programu.
getFreeLocations :: InterpretMonad [Location]
getFreeLocations = gets freeLocations

-- Funkcja zwracająca aktualne środowisko programu.
getEnvironment :: InterpretMonad Environment
getEnvironment = gets environment

-- Początkowy pusty stan programu.
initialState = ProgramState initialStore initialFreeLocations initialEnvironment

-- Monada służąca do interpretacji programu.
type InterpretMonad a = StateT ProgramState (ExceptT String IO) a
runInterpretMonad :: InterpretMonad a -> ProgramState -> IO (Either String (a, ProgramState))
runInterpretMonad m state = runExceptT (runStateT m state)

-------------------------------------------------------------------------------------------
-- Funkcje pomocnicze.
-------------------------------------------------------------------------------------------

-- TODO Zwalnianie lokacji pamięci ???

-- Funkcja aktualizująca pamięć programu o nową wartość pod zadaną lokacją.
updateStore :: Location -> Object -> InterpretMonad ()
updateStore location object = modify $ \state -> 
    state { store = Map.insert location object (store state) }

-- Funkcja ustalająca aktualne wolne lokacje programu na zadane argumentem.
putFreeLocations :: [Location] -> InterpretMonad ()
putFreeLocations freeLocations = modify $ \r -> 
    r { freeLocations }

-- Funkcja zwracająca następną wolną lokację oraz usuwająca ją z listy wolnych lokacji programu.
alloc :: InterpretMonad Location
alloc = do
    freeLocations <- getFreeLocations
    case freeLocations of
        [] -> throwError $ "Memory error: no free locations."
        (location:locations) -> do
            putFreeLocations locations
            return location

-- Funkcja ustalająca aktualne środowisko na zadane.
putEnvironment :: Environment -> InterpretMonad ()
putEnvironment environment = modify $ \r -> 
    r { environment }

-- Funkcja aktualizująca aktualne środowisko zadanym przekształceniem.
modifyEnvironment :: (Environment -> Environment) -> InterpretMonad ()
modifyEnvironment f = do
    environment <- getEnvironment 
    putEnvironment $ f environment

-- Funkcja dokładająca do środowiska nowy identyfikator o zadanej lokacji.
updateEnvironment :: Ident -> Location -> Environment -> Environment
updateEnvironment ident location = Map.insertWith (++) ident [location] 

-- Funkcja zwracająca lokację zadanego identyfikatora.
lookupEnvironment :: Ident -> Environment -> Maybe Location
lookupEnvironment ident environment = do
    locations <- Map.lookup ident environment 
    case locations of
        [] -> Nothing
        (location:_) -> return location

-- Funkcja zwracająca lokację, na którą wskazuje zadany identyfikator.
getLocation :: Ident -> InterpretMonad Location
getLocation ident =  do
    environment <- getEnvironment
    case (lookupEnvironment ident environment) of
        Nothing -> let (Ident i) = ident in 
            throwError $ "`" ++ i ++ "` was not declared in this scope."
        (Just location) -> return location

-- Funkcja zwracająca obiekt, na który wskazuje zadany identyfikator.
getObject :: Ident -> InterpretMonad Object
getObject ident =  do
    location <- getLocation ident
    store <- getStore 
    case (Map.lookup location store) of
        Nothing -> let (Ident i) = ident in 
            throwError $ "`" ++ i ++ "` was not allocated in this scope."
        (Just object) -> return object