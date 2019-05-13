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

-- Funkcja zwracająca typ danej wartości.
getValueType :: Value -> Type
getValueType (ValueInteger _) = Int
getValueType (ValueBool _) = Bool
getValueType ValueVoid = Void
getValueType (ValueString _) = Str

-- Funkcja zwracająca domyślna wartość zadanego typu.
getDefaultValue :: Type -> Value
getDefaultValue Int = ValueInteger 0
getDefaultValue Bool = ValueBool False
getDefaultValue Void = ValueVoid
getDefaultValue Str = ValueString ""

-- Definicja typu przechowywanego w pamięci stanu programu.
data Object = ObjectValue Value | ObjectFunDef FunDef Environment
    deriving(Eq, Ord, Show, Read)

-- Definicja typu aktualnej głębokości drzewa interpretacji.
type Depth = Int

-- Definicja początkowej głębokości drzewa.
initialDepth :: Depth
initialDepth = 0

-- Definicja typu pamięci programu.
type Store = Map.Map Location Object

-- Definicja początkowej pustej pamięci programu.
initialStore :: Store
initialStore = Map.empty

-- Definicja typu lokację w pamięci stanu programu.
type Location = Int

-- Definicja początkowo wolnych lokacji.
initialFreeLocations :: [Location]
initialFreeLocations = [1..30]

-- Definicja typu środowiska programu.
data IdentInfo = Info Location Type Depth
    deriving(Eq, Ord, Show, Read)

type Environment = Map.Map Ident IdentInfo

-- Definicja początkowo pustego środowiska programu.
initialEnvironment :: Environment
initialEnvironment = Map.empty

-- Struktura przechowująca aktualny stan programu.
data ProgramState = ProgramState { 
    depth :: Depth,
    store :: Store, 
    freeLocations :: [Location], 
    environment :: Environment }


-- Funkcja zwracająca aktualną głębokość programu.
getDepth :: InterpretMonad Depth
getDepth = gets depth

-- Funkcja ustalająca aktualną głębokość programu na zadaną argumentem.
putDepth :: Depth -> InterpretMonad ()
putDepth depth = modify $ \r -> 
    r { depth }

-- Funkcja zwracająca aktualną pamięć programu.
getStore :: InterpretMonad Store  
getStore = gets store

-- Funkcja zwracająca aktualne wolne lokacje programu.
getFreeLocations :: InterpretMonad [Location]
getFreeLocations = gets freeLocations

-- Funkcja zwracająca aktualne środowisko programu.
getEnvironment :: InterpretMonad Environment
getEnvironment = gets environment

-- Początkowy pusty stan programu.
initialState = ProgramState initialDepth initialStore initialFreeLocations initialEnvironment

-- Monada służąca do interpretacji programu.
type InterpretMonad a = StateT ProgramState (ExceptT String IO) a
runInterpretMonad :: InterpretMonad a -> ProgramState -> IO (Either String (a, ProgramState))
runInterpretMonad m state = runExceptT (runStateT m state)

-------------------------------------------------------------------------------------------
-- Funkcje pomocnicze.
-------------------------------------------------------------------------------------------

releaseLocation :: Location -> InterpretMonad ()

-- Funkcja zwalniająca pamięć pod zadaną lokacją.
releaseLocation location = do 
    locations <- getFreeLocations
    putFreeLocations (location:locations)

releaseLocations :: [Location] -> InterpretMonad ()

-- Funkcja zwalniająca pamięć pod zadanymi lokacjami.
releaseLocations [] = do
    return ()

releaseLocations (location:locations) = do
    releaseLocation location
    releaseLocations locations

releaseDifference :: Environment -> Environment -> InterpretMonad ()
releaseDifference environmentBefore environmentAfter = do
    let diff = Map.differenceWith (\ x y -> if x == y then Nothing else (Just x)) environmentAfter environmentBefore in do
        releaseLocations $ map (\(Info l _ _) -> l) (Map.elems diff)

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
        [] -> throwError $ "Ino Exception: Memory error, no free locations left."
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
updateEnvironment :: Ident -> IdentInfo -> Environment -> Environment
updateEnvironment ident identInfo = Map.insert ident identInfo

-- Funkcja zwracająca lokację zadanego identyfikatora.
lookupEnvironment :: Ident -> Environment -> Maybe IdentInfo
lookupEnvironment ident environment = do
    Map.lookup ident environment 

-- Funkcja może zwracająca informacje związane z identyfikatorem, na którą wskazuje zadany identyfikator.
getMaybeIdentInfo :: Ident -> InterpretMonad (Maybe IdentInfo)
getMaybeIdentInfo ident = do
    environment <- getEnvironment
    return $ lookupEnvironment ident environment

-- Funkcja zwracająca informacje związane z identyfikatorem, na którą wskazuje zadany identyfikator, lub error jeśli nie istnieje.
getIdentInfo :: Ident -> InterpretMonad IdentInfo
getIdentInfo ident = do
    maybeIdentInfo <- getMaybeIdentInfo ident
    case maybeIdentInfo of
        Nothing -> let (Ident i) = ident in 
            throwError $ "Ino Exception: `" ++ i ++ "` was not declared in this scope."
        (Just identInfo) -> return identInfo

-- Funkcja zwracająca obiekt, na który wskazuje zadany identyfikator.
getObject :: Ident -> InterpretMonad Object
getObject ident =  do
    (Info location _ _) <- getIdentInfo ident
    store <- getStore 
    case (Map.lookup location store) of
        Nothing -> let (Ident i) = ident in 
            -- To nie może się zdarzyć.
            throwError $ "Ino Exception: `" ++ i ++ "` was not allocated in this scope."
        (Just object) -> return object

putObject :: Ident -> Type -> Depth -> Object -> InterpretMonad ()
putObject ident t depth object = do
    location <- alloc
    modifyEnvironment (updateEnvironment ident (Info location t depth))
    updateStore location object

-- Funkcja dodająca pod zadany identyfikator nowy obiekt na zadanej głębokości.
declareObject :: Ident -> Type -> Object -> InterpretMonad ()
declareObject ident t object = do
    maybeIdentInfo <- getMaybeIdentInfo ident
    actualDepth <- getDepth

    case maybeIdentInfo of
        Nothing -> do
            putObject ident t actualDepth object
        (Just (Info _ _ declarationDepth)) -> do
            case (declarationDepth < actualDepth) of
                True -> putObject ident t actualDepth object
                False -> let (Ident i) = ident in throwError $ "Ino Exception: Redeclaration of `" ++ i ++ "`."