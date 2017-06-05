module Gomoku.ListImpl where

import Gomoku.Abstractions
import qualified Data.List as L


data ListBoard = ListBoard [[Field]]

instance Board ListBoard where
    blankBoard len = ListBoard $ L.replicate len $ L.replicate len Blank
    updateBoard (ListBoard board) (Move x y player) =
        ListBoard $ (take y board) ++ [updatedLine] ++ (take (length board - y+1) $ drop (y+1) board)
        where
            line = board !! y
            updatedLine = (take x line) ++ [Player player] ++ (take (length line - x) $ drop (x+1) line)
    genMoves (ListBoard board) player =
        map (\(x,y) -> Move x y player) availableFields
        where
            len = length board
            availableFields = [(x, y)| x <- [0..(len - 1)],
                                       y <- [0..(len - 1)],
                                       board !! y !! x == Blank]
    genNeighboringMoves (ListBoard board) distance player =
        map (\(x,y) -> Move x y player) availableFields
        where
            len = length board
            availableFields = [(x, y)| x <- [0..(len - 1)],
                                       y <- [0..(len - 1)],
                                       board !! y !! x == Blank,
                                       let neighbours = [board !! neighbourY !! neighbourX | neighbourX <- [max (x-distance) 0..min (x+distance) (len-1)],
                                                                                             neighbourY <- [max (y-distance) 0..min (y+distance) (len-1)]],
                                       any (/= Blank) neighbours
                                       ]

    evaluatePlayer board player = PlayerEvaluation {
        fives = xss 5,
        fours = xss 4,
        threes = xss 3,
        doubles = xss 2
    }
      where
        xss n = sum $ fmap (openXsInLine n player) $ allLines board

    evaluateBoard board = BoardEvaluation {
        black = evaluatePlayer board Black,
        white = evaluatePlayer board White
    }



instance Show ListBoard where
  show board = concat $ drawBoard board
    where
        drawField :: Field -> Char
        drawField field = case field of
                            Player Black -> 'X'
                            Player White -> 'O'
                            Blank -> ' '
        drawLine :: [Field] -> String
        drawLine line = L.intersperse '|' $ fmap (drawField) line
        drawBoard (ListBoard board) = L.intersperse "\n" $ fmap (drawLine) board


exampleBoard :: ListBoard
exampleBoard = foldl (updateBoard) (blankBoard 19) moves
    where
        moves = [
                    (Move 3 3 Black),
                    (Move 4 4 White),
                    (Move 2 4 Black),
                    (Move 3 4 White),
                    (Move 4 2 Black),
                    (Move 3 5 White),
                    (Move 5 1 Black),
                    (Move 2 3 White)
                ]



diagonals :: [[a]] -> [[a]]
diagonals board =
    diagonals' board []
    where
        diagonals' [] [] = []
        diagonals' [] openLines = (diagonal openLines) : diagonals' [] (filter (isListNotEmpty) (fmap (tail) openLines))
        diagonals' (line:lines) openLines = (diagonal newOpenLines) : (diagonals' lines (openLinesTails))
            where
                newOpenLines = openLines ++ [line]
                openLinesTails = filter (isListNotEmpty) (fmap (tail) newOpenLines)
        diagonal openLines = fmap (head) openLines
        isListNotEmpty [] = False
        isListNotEmpty _ = True


allLines :: ListBoard -> [[Field]]
allLines (ListBoard board) =
    board ++
    (L.transpose board) ++
    (diagonals board) ++
    (diagonals (L.reverse board))


-- produce all combinations of n elements of value a and m elements of value b
-- ex. combinations 'a' 2 'b' 3 == ["aabbb","ababb","abbab","abbba","baabb","babab","babba","bbaab","bbaba","bbbaa"]

combinations :: a -> Int -> a -> Int -> [[a]]
combinations _ 0 b m = [replicate m b]
combinations a n _ 0 = [replicate n a]
combinations a n b m = [ [a] ++ l | l <- combinations a (n-1) b m ] ++ [ [b] ++ l | l <- combinations a n b (m-1)]

-- generate list of lists segments of length 'len'
-- ie. subsegments 3 "abcdefg" == ["abc", "bcd", "cde", "def", "efg"]
subsegments :: Int -> [a] -> [[a]]
subsegments len line = [L.take len $ L.drop d line | d <- [0..((length line) - len)]]

-- count open 'Xs' (doubles, threes, fours, fives) for player in given line
openXsInLine :: Int -> Player -> [Field] -> Int
openXsInLine x player line =
  countMatches
  where
    xsPatterns = combinations Blank (5 - x) (Player player) x
    prefixPatterns = [ pattern ++ [notPlayer] | pattern <- xsPatterns,
                                                notPlayer <- notPlayerField player ]
    suffixPatterns = [ [notPlayer] ++ pattern | pattern <- xsPatterns,
                                                notPlayer <- notPlayerField player ]
    infixPatterns = [ [notPlayerFirst] ++ pattern ++ [notPlayerSecond] | pattern <- xsPatterns,
                                                                            notPlayerFirst <- notPlayerField player,
                                                                            notPlayerSecond <- notPlayerField player ]
    matchPrefix pattern = pattern `L.isPrefixOf` line
    matchSuffix pattern = pattern `L.isSuffixOf` line
    matchInfix segment pattern = pattern == segment
    matchInfixSegments pattern = fmap (\match -> match pattern) $ fmap (matchInfix) $ subsegments 7 line
    matchExact pattern = pattern == line
    allMatches =
        fmap (matchPrefix) prefixPatterns ++
        fmap (matchSuffix) suffixPatterns ++
        fmap (matchExact) xsPatterns ++
        (concat $ fmap (matchInfixSegments) infixPatterns)
    countMatches = sum $ fmap (\match -> if match then 1 else 0) allMatches

