# Reprezentacja stanu gry

Struktury do reprezentacji stanu gry:

- Gracz: czarny, biały
- Pole: puste, zajęte przez gracza
- Plansza: dwuwymiarowa kolekcja pól
  - Lista
    - indexing
      - (!) - O(n)
  - Data.Vector
    - indexing
      - (!), (!?) - O(1)
  - Data.Vector.Unboxed
    - indexing - O(1)
  - Data.Matrix
    - indexing:
       - (!), getElem - O(1)
  - Data.Sequence (FingerTree)
    - indexing - O(1) - O(log n)
  - Data.Sequences - SemiSequence https://hackage.haskell.org/package/mono-traversable-1.0.2/docs/Data-Sequences.html#v:isInfixOf
  - Data.Array
  - Data.Map


# [DONE] Wyświetlanie stanu gry, wczytywanie stanu gry

 | | | | | | | | | | | | | | | | | |
 | | | | |X| | | | | | | | | | | | |
 | | | |X| | | | | | | | | | | | | |
 | |O|X| | | | | | | | | | | | | | |
 | |X|O|O| | | | | | | | | | | | | |
 | | |O| | | | | | | | | | | | | | |
 | | | | | | | | | | | | | | | | | |
 | | | | | | | | | | | | | | | | | |
 | | | | | | | | | | | | | | | | | |
 | | | | | | | | | | | | | | | | | |
 | | | | | | | | | | | | | | | | | |
 | | | | | | | | | | | | | | | | | |
 | | | | | | | | | | | | | | | | | |
 | | | | | | | | | | | | | | | | | |
 | | | | | | | | | | | | | | | | | |
 | | | | | | | | | | | | | | | | | |
 | | | | | | | | | | | | | | | | | |
 | | | | | | | | | | | | | | | | | |
 | | | | | | | | | | | | | | | | | |


# [DONE] Wykrywanie wygranej
# [DONE] Generowanie listy następnych ruchów
  - Dowolne niezajęte pole
  - Warunek: bezpośrednie sąsiedztwo z zajętym polem
  - Warunek: obecność zajętego pola w odległości najwyzej dwóch pól
# [DONE] Generowanie drzewa ruchów
# [DONE] Funkcja oceny stanu gry
  - Wykrywanie wzorców
     - piątki
      - prefix: 1 * 2 wzorców = 2
       - suffix: 1 * 2 wzorców = 2
       - infix: 1 * (2*2) wzorców = 4
     - czwórki
       - prefix: 5 * 2 wzorców = 10
       - suffix: 5 * 2 wzorców = 10
       - infix: 5 * (2*2) wzorców = 20
     - trójki
       - prefix: 10 * 2 wzorców = 20
       - suffix: 10 * 2 wzorców = 20
       - infix: 10 * (2*2) wzorców = 40
     - dwójki
       - prefix: 10 * 2 wzorców = 20
       - suffix: 10 * 2 wzorców = 20
       - infix: 10 * (2*2) wzorców = 40
# [DONE] Implementacja algorytmu Minimax
# [DONE] Pętla gry i interakcja z uzytkownikiem via cli
# Optymalizacje
  - [DONE] Data.Vector.Unboxed
  - [DONE] reprezentacja mapy jako Data.Vector.Unboxed intów, gdzie pole jest oznaczane jako 2 bity w int na odpowiedniej pozycji
  - [TODO] Alfa-beta pruning
  - [TODO] Modyfikacja boardu zamiast kopiowania podczas przechodzenia drzewa gry
  - [TODO] modyfikacja mozliwych ruchów podczas przechodzenia drzewa gry zamiast generowania za kazdym razem


# Zasady

1. Rozmiar
    - 15x15 pól
    - 19x19 pól
2. Warunek wygranej
    - Ułozenie pięciu kamieni w jednej lini tak, aby nie były zablokowane na obu końcach
3. Rozpoczęcie gry
    - Czarne zaczynają w dowolnym polu planszy, następnie ruch wykonuje gracz biały
4. Algorytm wyboru następnego ruchu:
  1. Czy jest taki ruch, ze wygrywam? Tak - wykonaj ten ruch
  2. Czy jest taki ruch, ze przeciwnik wygrywa?
    Tak - Czy mogę zablokować ten ruch?

# Sources:
1. Memoization https://wiki.haskell.org/Memoization
2. Go-Moku and Threat-Space Search - algorytm nie uzyty w implementacji, ale pozwalający na due ulepszenie silnika AI
  - http://www.renju.nu/wp-content/uploads/sites/46/2016/09/Go-Moku.pdf
  - https://www.mimuw.edu.pl/~awojna/SID/referaty/piechna/tss.html
  - http://rab.ict.pwr.wroc.pl/~abogdzie/gomoku/
3. Stream Fusion
  - https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/haskell-beats-C.pdf
4. Profilowanie
  - https://www.fpcomplete.com/blog/2015/04/ghc-prof-flamegraph

    compile: ghc --make -auto-all -prof -rtsopts <sources>
    run:     <binary> +RTS -p -RTS
             cat <binary>.prof | ghc-prof-flamegraph > <binary>.folded
             <binary>.folded | flamegraph.pl > <binary>.svg
5. Alpha-Beta pruning:
  - http://www.geeksforgeeks.org/minimax-algorithm-in-game-theory-set-4-alpha-beta-pruning/
  - http://web.cs.ucla.edu/~rosen/161/notes/alphabeta.html
  - http://inst.eecs.berkeley.edu/~cs61b/fa14/ta-materials/apps/ab_tree_practice/
  - https://hackage.haskell.org/package/game-tree-0.1.0.0


Applicative

(+) <$> (Just 5) <*> (Just 6)

# Data.Tree.Pretty drawVerticalTree


# GUI

## Web-based

### Contracts

- Start a new game
  - Request
    - player colour
  - Response:
    - game id
- Make a move
  - Request
    - game id
    - Move x y
  - Response
    - Game state

### Web ui operations

- Select player (Black, White)
- Make a move (x, y)