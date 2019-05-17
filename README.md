# Uruchomienie

## Zbudowanie interpretera.
```bash
make && make clean
```

Gdyby rozwiązanie miało być budowane na students, w pliku Makefile należy zmienić:
```bash
bnfc Ino.ebnf 
# na
/home/students/inf/PUBLIC/MRJP/bin/bnfc Ino.ebnf
```

Oraz:
```bash
ghc --make Interpreter.hs -o Interpreter 
# na
/home/students/inf/PUBLIC/MRJP/ghc-8.2.2/bin/ghc --make Interpreter.hs -o Interpreter
```

GHC na students wymaga jeszcze doinstalowania mtl dla pakietu Control.Monad:
```bash
/home/students/inf/PUBLIC/MRJP/ghc-8.2.2/bin/cabal update
/home/students/inf/PUBLIC/MRJP/ghc-8.2.2/bin/cabal install mtl
```

## Uruchomienie testów interpretera na plikach przykładowych.
Skrypt wypisuje na konsolę wszytskie testy w kolejności alfabetycznej, oraz efekt działania interpretera na nich (najpierw good/, potem bad/).
```bash
chmod+x runTests.sh  
./runTests  
```

# Opis rozwiązania

## Parsowanie
Odbywa się przy użyciu generatora parserów bnfc.
W wygenerowanym pliku AbsIno.hs znajduje się zbiór kategorii semantycznych języka.

## InterpreterLib
Biblioteka zawierająca Interpretery poszczególnych kategorii semantycznych języka.
Każdy statement może zwrócić: wartość, brak wartości, break, lub continue.
Każdy blok (również wywołanie funkcji, ciało if/else, ciało while) wykonuje kopię aktualnego środowiska z przed wejścia, którą po opuszczeniu bloku przywraca z powrotem. Jednocześnie zwiększa głębokość drzewa interpretacji. Głębokość interpretacji jest pamiętana, żeby wykrywać redeklaracje identyfikatorów w tym samym bloku.

## InterpreterStateLib
Zbiór definicji i funkcji pomocniczych dla stanu Interpretera. W każdym momencie interpretacji, stan składa się z monady state zawierającej: aktualne środowisko zmiennych, dostępnych wolnych lokacji, pamięci programu, oraz aktualnej głębokości drzewa interpretacji.
Środowisko to mapowanie z identyfikatora w trójkę: lokacja, typ, głębokość ostatniej deklaracji.
Pamięć to mapowanie z lokacji w wartość.

## TypeChecker
Biblioteka zawierająca TypeCheckery poszczególnych kategorii semantycznych języka.
TypeChecker wygląda podobnie do interpretera, jednak nie wylicza konkretnych wartości, a operuje na typach.
Sprawdza poprawności przypisań, przekazań argumentów do funkcji, liczb argumentów w funkcjach, typów zwracanych wartości. Przechodzi po wszystkich gałęziach ifa, ciało pętli while sprawdza raz, więc błąd typu za nieskończoną pętlą będzie oczywiście wyłapany.

## TypeCheckerStateLib
Zbiór definicji i funkcji pomocniczych dla stanu TypeCheckera. W każdym momencie sprawdzania typów, stan składa się monady state zawierającej: aktualne środowisko zmiennych, nazwy funkcji w której aktualnie odbywa się sprawdzanie typów, typu z zwracanego wyniku funkcji w której aktualnie odbywa się sprawdzanie oraz z aktualnej głębokości drzewa sprawdzania typów.
Środowisko to mapowanie identyfikatora w parę (dla zmiennej): typ, głębokość inicjalizacji albo trójkę (dla funkcji), typ wyniku, typy argumentów, głębokość ostatniej deklaracji.

## PrintHelper
Zbiór funkcji pomocniczych do wypisywania różnych typów wartości.

## TupleHelper
Zbiór funkcji pomocniczych dla operacji na tuplach.

# Zrealizowane konstrukcje

Wszystkie funkcjonalności do 20pkt. 

W punkcie 7. wybrano przekazywanie parametrów przez wartość/referencję. Składnia jak w C++, z użyciem &. 

(4pkt) Statyczne typowanie.

(2pkt) Dowolnie zagnieżdżone definicje funkcji / procedur z zachowaniem poprawności statycznego wiązania identyfikatorów, przykład w good/good5.

(1pkt) Operacje przerywające pętlę while - break i continue, przykład w good/good8.

(2pkt) Dowolnie zagnieżdżone krotki z przypisaniem. Składnia podobna do C++, przykład w good/good9.

(Dodatkowo) Nie było umieszczone w punktowanych funkcjonalnościach, ani w projekcie języka, interpreter posiada odśmiecacz zmiennych z pamięci, przykład działania w good/good10.