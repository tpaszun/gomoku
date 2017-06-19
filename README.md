# Instrukcja kompilacji

## Stack

Aby skompilować i uruchomić program w trybie gry "Human vs AI" (gracz zaczyna grę) należy wykonać polecenia:

```
> stack build
> stack exec gomoku
```

# Instrukcja kompilacji i uruchomienia programu konkursowego

## Stack

Aby skompilować i uruchomić program w trybie konkursowym należy wykonać polecenia:


```
> stack build
> stack exec -- contest b <f1 >f2
> stack exec -- contest w >f1 <f2
```

Pliki binarne zbudowane przez `stack` znajdują się w katalogu: `.stack-work/install/x86_64-osx/lts-8.16/8.0.2/bin/contest`

# Opis

Pełen kod źródłowy dostępny pod adresem: https://github.com/tpaszun/gomoku

## Reprezentacja planszy (`Gomoku.BitBoard`, `Gomoku.BitBoardImpl`)

Plansza jest przechowywana jako wektor (`Data.Vector.Unboxed.Vector`) wszystkich lini planszy - poziomych, pionowych oraz przekątnych. Każdy element wektora (`Word64`) zawiera pojedynczą linię. Pola w lini kodowane są przy użyciu dwóch bitów na pole.

Motywacja konstrukcji planszy w postaci bitowej:
  - kopiowanie planszy po modyfikacji jako kopiowanie ciągłego bloku pamięci
  - dopasowywanie wzorców jako operacje bitowe (XOR, AND, SHIFT są operacjami wykonywanymi bardzo efektywnie przez CPU)
  - możliwość użycia "stream fusion" dla operacji przeglądania planszy

Dla polepszenia czytelności możliwe jest silne typowanie bytów reprezentowanych przez typy proste (`newtype Line = Line Word64`, `newtype Pattern = Pattern Word64`). Najprawdopodobniej nie będzie miało to wpływu na wydajność, gdyż kompilator GHC podczas kompilacji zamienia typy `newtype` na typy proste użyte w definicji.

## Wzorce i dopasowywanie (`Gomoku.Patterns`, `Gomoku.PatternCache`, `Gomoku.PatternMatching`)

Wzorce w postaci binarnej generowane są podczas pierwszego ich użycia i cache'owane są w postaci wektora (`Data.Vector.Unboxed.Vector Word64`). Dopasowanie wzorca polega na odpowiednim przesunięciu bitowym lini oraz przycięciu (bitowy AND) lini i wykonaniu bitowego XORa - jeżeli wzorzec jest identyczny z dopasowywaną sekcją lini to wynik będzie równy 0.

## Wykrywanie oczywistych ruchów

Program zawiera funkcje wykrywające oczywiste ruchy gracza:
  - ruch wygrywający (ułożenie pięciu kamieni)
  - ruch nie-przegrywający (zablokowanie ułożenia pięciu kamieni przez przeciwnika)
  - ruch wygrywający w następnej turze (ułożenie czterech kamieni w lini niebronionych na obu końcach, lub ułożenie przy pomocy jednego ruchu dwóch zwykłych czwórek)
  - ruch blokujący ruch przeciwnika wygrywający w następnej turze

## Konstrukcja drzew

### Drzewo najlepszego ruchu przeglądane algorytmem minimax (`Gomoku.AI`)

Program wykorzystuje algorytm minimax do przeglądania drzewa najlepszych ruchów. Do algorytmu minimax została dodana modyfikacja, która powoduje głębsze przeglądanie węzłów, które zawierają oczywisty ruch (jeżeli funkcja min/max natrafi na węzeł z jednym potomkiem to nie dekrementuje licznika głębokości). Funkcja generująca drzewo najlepszych ruchów wykorzystuje funkcje znajdujące oczywiste ruchy do redukcji ilości generowanych gałęzi (nie generuje oczywiście złych gałęzi). Gałęzienie drzewa najlepszych ruchów wykorzystuje:
  - selekcję pól sąsiadujących z zajętymi polami w odległości co najwyżej dwóch pól,
  - szybką estymatę wartości pola (wartość pola dla przeciwnika + wartość pola dla gracza) - bazującą na wiedzy, że pole, które jest najlepsze dla przeciwnika to również pole, które jest najlepsze dla gracza (gomoku jest grą o sumie zerowej),
  - wybór `n` pól o najwyższej estymowanej wartości.

### Drzewo sekwencji wygrywających zagrożeń (`Gomoku.ThreatSearch`)

Do programu dodałem funkcję generującą drzewo sekwencji zagrożeń które prowadzą do wygranej. To drzewo mozna przeglądać głębiej niż drzewo najlepszych ruchów przez co program jest silniejszy (o ile standardowy algorytm selekcji ruchów stworzy taki układ pionów na planszy, że zawierają wygrywające sekwencje)

Program nie implementuje algorytmu "Threat Space Search" - natomiast zaimplementowane rozwiązanie jest zainspirowane materiałami dotyczącymi TSS.

# Możliwe rozszerzenia programu:

- Zastosowanie lepszego algorytmu przeglądania drzewa najlepszych ruchów, np. alpha-beta pruning, negascout, proof-number search, etc.
- Modyfikacja funkcji ewaluacji ruchu/planszy
- Zaimplementowanie pełnego algorytmu Threat Space Search
- Optymalizacje wydajności po profilowaniu programu, np.
    - użycie mutowalnej reprezentacji planszy podczas generowania drzew (mutacja planszy przy przechodzeniu głębiej oraz cofanie ruchów przy powrocie do korzenia) - niemutowalna reprezentacja pociąga za sobą konieczność kopiowania bloków pamięci całej planszy
    - zapisanie długości lini oraz wzorca w wartości `Word64` reprezentującej linię/wzorzec zamiast wyliczania przez osobne funkcje - SHIFT i AND mogą okazać się szybsze niz "function call"
