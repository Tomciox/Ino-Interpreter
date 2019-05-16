-------------------------------
-- Autor:       Tomasz Madej --
-- Nr. albumu   385853       --
-------------------------------

{-# LANGUAGE MultiParamTypeClasses, NamedFieldPuns#-}

module InterpreterStateLib where 

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
-- Typy używane przy interpretacji programu.
-------------------------------------------------------------------------------------------

-- Typ wyniku interpretaci statementu. Używany do kontrolowania przepływu sterowania programem. 
data ResultReturn = ResultUnit | ResultBreak | ResultContinue | ResultValue Value

-- Typ wartości argumentu funkcji w trakcie parsowania. 
-- Jeśli argument jest przekazywany przez wartość, to jest wyliczany.
-- Jeśli przez referencję, przekazywana jest jedynie lokacja.
data FunArgVal = FunArgValue Value | FunArgLocation Location

-- Typ wyniku interpretacji wyrażeń: Int, Bool, Void, String lub Tuple.
data Value = ValueInteger Integer | ValueBool Bool | ValueVoid | ValueString String | ValueTuple [Value]
    deriving(Eq, Ord, Show, Read)

-- Zwrócenie typu zadanej wartości.
getValueType :: Value -> Type
getValueType value = case value of
    (ValueInteger _) -> Int
    (ValueBool _) -> Bool
    ValueVoid -> Void
    (ValueString _) -> Str
    (ValueTuple ts) -> Tuple (foldr (\t ts -> ((getValueType t):ts)) [] ts)

-- Zwrócenie domyślnej wartości zadanego typu.
getDefaultValue :: Type -> Value
getDefaultValue t = case t of
    Int -> ValueInteger 0
    Bool -> ValueBool False
    Void -> ValueVoid
    Str -> ValueString ""
    (Tuple ts) -> ValueTuple (foldr (\t ts -> ((getDefaultValue t):ts)) [] ts)

-- Typ obiektu przechowywanego w pamięci stanu programu: wartość lub funkcja.
data Object = ObjectValue Value | ObjectFunDef FunDef Environment
    deriving(Eq, Ord, Show, Read)

-------------------------------------------------------------------------------------------
-- Typy używane do przechowywania stanu interpretacji programu.
-------------------------------------------------------------------------------------------

-- Aktualnej głębokość drzewa interpretacji.
type Depth = Int

-- Definicja początkowej głębokości drzewa interpretacji.
initialDepth :: Depth
initialDepth = 0

--  Pamięć programu.
type Store = Map.Map Location Object

-- Definicja początkowej pustej pamięci programu.
initialStore :: Store
initialStore = Map.empty

-- Lokacja w pamięci stanu programu.
type Location = Int

-- Definicja początkowo wolnych lokacji.
initialFreeLocations :: [Location]
initialFreeLocations = [1..10000]

-- Informacje o obiekcie przechowywane w środowisku.
data IdentInfo = Info Location Type Depth
    deriving(Eq, Ord, Show, Read)

-- Środowisko zmiennych i funkcji programu.
type Environment = Map.Map Ident IdentInfo

-- Definicja początkowo pustego środowiska programu.
initialEnvironment :: Environment
initialEnvironment = Map.empty

-- Aktualny stan programu.
data ProgramState = ProgramState { 
    depth :: Depth,
    store :: Store, 
    freeLocations :: [Location], 
    environment :: Environment }

-- Zwrócenie aktualnej głębokości programu.
getDepth :: InterpretMonad Depth
getDepth = gets depth

-- Ustalenie aktualnej głębokości programu na zadaną argumentem.
putDepth :: Depth -> InterpretMonad ()
putDepth depth = modify $ \state -> 
    state { depth }

-- Zwrócenie aktualnej głębokości programu.
getStore :: InterpretMonad Store  
getStore = gets store

-- Zaktualizowanie pamięci programu o nową wartość pod zadaną lokacją.
updateStore :: Location -> Object -> InterpretMonad ()
updateStore location object = modify $ \state -> 
    state { store = Map.insert location object (store state) }

-- Zwrócenie listy wolnych lokacji.
getFreeLocations :: InterpretMonad [Location]
getFreeLocations = gets freeLocations

-- Ustalenie aktualnych wolnych lokacji programu na zadane argumentem.
putFreeLocations :: [Location] -> InterpretMonad ()
putFreeLocations freeLocations = modify $ \state -> 
    state { freeLocations }

-- Zwrócenie aktualnego środowiska programu.
getEnvironment :: InterpretMonad Environment
getEnvironment = gets environment

-- Ustalenie aktualnego środowiska programu na zadane argumentem.
putEnvironment :: Environment -> InterpretMonad ()
putEnvironment environment = modify $ \state -> 
    state { environment }

-- Funkcja aktualizująca aktualne środowisko o nowy identyfikator.
updateEnvironment :: Ident -> IdentInfo -> InterpretMonad ()
updateEnvironment ident info = modify $ \state -> 
    state { environment = Map.insert ident info (environment state) }

-- Początkowy pusty stan programu.
initialState = ProgramState initialDepth initialStore initialFreeLocations initialEnvironment

-------------------------------------------------------------------------------------------
-- Monada służąca do interpretacji programu.
-------------------------------------------------------------------------------------------

type InterpretMonad a = StateT ProgramState (ExceptT String IO) a

runInterpretMonad :: InterpretMonad a -> ProgramState -> IO (Either String (a, ProgramState))
runInterpretMonad m state = runExceptT (runStateT m state)

-------------------------------------------------------------------------------------------
-- Funkcje pomocnicze do operacji na stanie programu.
-------------------------------------------------------------------------------------------

-- Zwrócenie następnej wolnej lokacji oraz usunięcie jej z listy dostępnych wolnych lokacji programu.
alloc :: InterpretMonad Location
alloc = do
    freeLocations <- getFreeLocations
    case freeLocations of
        [] -> throwError $ "Ino Exception: Memory error, no free locations left."
        (location:locations) -> do
            putFreeLocations locations
            return location

-- Zwolnienie pamięci pod zadaną lokacją.
releaseLocation :: Location -> InterpretMonad ()
releaseLocation location = do 
    locations <- getFreeLocations
    putFreeLocations (location:locations)

-- Zwolnienie pamięci pod zadanymi lokacjami
releaseLocations :: [Location] -> InterpretMonad ()
releaseLocations ls = case ls of
    [] -> return ()
    (location:locations) -> do
        releaseLocation location
        releaseLocations locations

-- Zwolnienie lokacji używanych przez zmienne które się pojawiły w opuszczanym środowisku w stosunku do aktualnego.
releaseDifference :: Environment -> Environment -> InterpretMonad ()
releaseDifference environmentBefore environmentAfter =
    let diff = Map.differenceWith (\ x y -> if x == y then Nothing else (Just x)) environmentAfter environmentBefore in
        releaseLocations $ map (\(Info l _ _) -> l) (Map.elems diff)

-- Zwrócenie informacji związanych z identyfikatorem, na które wskazuje zadany identyfikator.
getMaybeIdentInfo :: Ident -> InterpretMonad (Maybe IdentInfo)
getMaybeIdentInfo ident = do
    environment <- getEnvironment
    return $ Map.lookup ident environment

-- Zwrócenie informacji związanych z identyfikatorem, na które wskazuje zadany identyfikator.
getIdentInfo :: Ident -> InterpretMonad IdentInfo
getIdentInfo ident = do
    maybeIdentInfo <- getMaybeIdentInfo ident
    case maybeIdentInfo of
        Nothing -> let (Ident i) = ident in 
            throwError $ "Ino Exception: `" ++ i ++ "` was not declared in this scope."
        (Just identInfo) -> return identInfo

-- Zwrócenie obiektu na który wskazuje zadany identyfikator.
getObject :: Ident -> InterpretMonad Object
getObject ident =  do
    (Info location _ _) <- getIdentInfo ident
    store <- getStore 
    case (Map.lookup location store) of
        Nothing -> let (Ident i) = ident in 
            -- Nie istnieją niezaalokowane zmienne.
            throwError $ "Ino Exception: `" ++ i ++ "` was not allocated in this scope."
        (Just object) -> return object

-- Dodanie nowego obiektu do środowiska oraz pamięci.
putObject :: Ident -> Type -> Depth -> Object -> InterpretMonad ()
putObject ident t depth object = do
    location <- alloc
    updateEnvironment ident (Info location t depth)
    updateStore location object

-- Deklaracja nowego obiektu o zadanym identyfikatorze.
declareObject :: Ident -> Type -> Object -> InterpretMonad ()
declareObject ident t object = do
    maybeIdentInfo <- getMaybeIdentInfo ident
    actualDepth <- getDepth
    case maybeIdentInfo of
        Nothing -> putObject ident t actualDepth object
        (Just (Info _ _ declarationDepth)) ->
            case (declarationDepth < actualDepth) of
                True -> putObject ident t actualDepth object
                False -> let (Ident i) = ident in throwError $ "Ino Exception: Redeclaration of `" ++ i ++ "`."