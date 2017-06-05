module Gomoku.VectorImpl where

import Gomoku.Abstractions
import qualified Data.Vector as V
import qualified Data.List as L

import Debug.Trace

isPrefixOf pattern line = pattern == V.slice 0 (V.length pattern) line
isSuffixOf pattern line = pattern == V.slice (V.length pattern - V.length pattern) (V.length pattern) line
subsegments len line = [V.slice d len line | d <- [0..((V.length line) - len)]]

data VectorBoard = VectorBoard (V.Vector (V.Vector Field))

instance Board VectorBoard where
    blankBoard len = VectorBoard $ V.replicate len $ V.replicate len Blank
    updateBoard (VectorBoard board) (Move x y player) = VectorBoard $ board V.// [(y, updatedLine)]
        where
            updatedLine = (board V.! y) V.// [(x, Player player)]
    genMoves (VectorBoard board) player =
        L.map (\(x,y) -> Move x y player) availableFields
        where
            len = V.length board
            availableFields = [(x, y)| x <- [0..(len - 1)],
                                       y <- [0..(len - 1)],
                                       board V.! y V.! x == Blank]
    genNeighboringMoves (VectorBoard board) distance player =
        map (\(x,y) -> Move x y player) availableFields
        where
            len = length board
            availableFields = [(x, y)| x <- [0..(len - 1)],
                                       y <- [0..(len - 1)],
                                       board V.! y V.! x == Blank,
                                       let neighbours = [board V.! neighbourY V.! neighbourX | neighbourX <- [max (x-distance) 0..min (x+distance) (len-1)],
                                                                                               neighbourY <- [max (y-distance) 0..min (y+distance) (len-1)]],
                                       any (/= Blank) neighbours]

instance Show VectorBoard where
  show board = L.concat $ drawBoard board
    where
        drawField :: Field -> Char
        drawField field = case field of
                            Player Black -> 'X'
                            Player White -> 'O'
                            Blank -> ' '
        drawLine :: V.Vector Field -> String
        drawLine line = L.intersperse '|' $ fmap (drawField) $ V.toList line
        drawBoard (VectorBoard board) = L.intersperse "\n" $ V.toList $ fmap (drawLine) board




exampleBoard :: VectorBoard
exampleBoard = L.foldl (updateBoard) (blankBoard 19) moves
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

diagonals :: V.Vector (V.Vector a) -> [V.Vector a]
diagonals board =
    diagonals' (V.toList board) []
    where
        diagonals' :: [V.Vector a] -> [V.Vector a] -> [V.Vector a]
        diagonals' [] [] = []
        diagonals' [] openLines = (diagonal openLines) : (diagonals' [] $ L.filter (not . V.null) $ L.map (V.tail) openLines)
        diagonals' (line:lines) openLines =
            (diagonal newOpenLines) :
            (diagonals' lines (openLinesTails))
            where
                newOpenLines = openLines ++ [line]
                openLinesTails =
                    L.filter (not . V.null) $
                    L.map (V.tail) newOpenLines
        diagonal :: [V.Vector a] -> V.Vector a
        diagonal openLines = V.fromList $ L.map (V.head) openLines


allLines :: VectorBoard -> [V.Vector Field]
allLines (VectorBoard board) =
    V.toList board ++
    (V.toList $ transpose board) ++
    (diagonals board) ++
    (diagonals (V.reverse board))

transpose :: V.Vector (V.Vector a) -> V.Vector (V.Vector a)
transpose matrix = V.map (\i -> V.map (V.! i) matrix) $ V.enumFromTo 0 (V.length matrix - 1)


-- produce all combinations of n elements of value a and m elements of value b
-- ex. combinations 'a' 2 'b' 3 == ["aabbb","ababb","abbab","abbba","baabb","babab","babba","bbaab","bbaba","bbbaa"]

combinations :: a -> Int -> a -> Int -> [[a]]
combinations _ 0 b m = [replicate m b]
combinations a n _ 0 = [replicate n a]
combinations a n b m = [ [a] ++ l | l <- combinations a (n-1) b m ] ++ [ [b] ++ l | l <- combinations a n b (m-1)]


openXsInLine :: Int -> Player -> V.Vector Field -> Int
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
    listPatternsToVector :: [[a]] -> [(V.Vector a)]
    listPatternsToVector = L.map (V.fromList)
    matchPrefix pattern = pattern `isPrefixOf` line
    matchSuffix pattern = pattern `isSuffixOf` line
    matchInfix segment pattern = pattern == segment
    matchInfixSegments pattern = fmap (\match -> match pattern) $ fmap (matchInfix) $ subsegments 7 line
    matchExact pattern = pattern == line
    allMatches =
        (L.map (matchPrefix) $ listPatternsToVector prefixPatterns) ++
        (L.map (matchSuffix) $ listPatternsToVector suffixPatterns) ++
        (L.map (matchExact) $ listPatternsToVector xsPatterns) ++
        (concat $ fmap (matchInfixSegments) $ listPatternsToVector infixPatterns)
    countMatches = sum $ fmap (\match -> if match then 1 else 0) allMatches

-- patterns:
---- player, prefix/suffix/infix/exact, fives/fours/threes/doubles
