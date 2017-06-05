{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Gomoku.UnboxedVectorImpl where

import Gomoku.Abstractions

import Data.Vector.Unboxed.Deriving
import Data.Word
import Data.Vector.Unboxed.Base (Unbox)

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.List as L

import Debug.Trace


fieldToWord8 :: Field -> Word8
fieldToWord8 Blank = 0
fieldToWord8 (Player Black) = 1
fieldToWord8 (Player White) = 2

word8ToField :: Word8 -> Field
word8ToField 0 = Blank
word8ToField 1 = Player Black
word8ToField 2 = Player White

derivingUnbox "Field"
  [t| Field -> Word8 |]
  [| fieldToWord8 |]
  [| word8ToField |]


isPrefixOf pattern line = pattern == U.slice 0 (min (U.length pattern) (U.length pattern)) line
isSuffixOf pattern line = pattern == U.slice (max 0 (U.length line - U.length pattern)) (U.length pattern) line


data UnboxedVectorBoard = UnboxedVectorBoard (V.Vector (U.Vector Field))

instance Board UnboxedVectorBoard where
    blankBoard len = UnboxedVectorBoard $ V.replicate len $ U.replicate len Blank
    updateBoard (UnboxedVectorBoard board) (Move x y player) = UnboxedVectorBoard $ board V.// [(y, updatedLine)]
        where
            updatedLine = (board V.! y) U.// [(x, Player player)]
    genMoves (UnboxedVectorBoard board) player =
        L.map (\(x,y) -> Move x y player) availableFields
        where
            len = V.length board
            availableFields = [(x, y)| x <- [0..(len - 1)],
                                       y <- [0..(len - 1)],
                                       board V.! y U.! x == Blank]
    genNeighboringMoves (UnboxedVectorBoard board) distance player =
        map (\(x,y) -> Move x y player) availableFields
        where
            len = length board
            availableFields = [(x, y)| x <- [0..(len - 1)],
                                       y <- [0..(len - 1)],
                                       board V.! y U.! x == Blank,
                                       let neighbours = [board V.! neighbourY U.! neighbourX | neighbourX <- [max (x-distance) 0..min (x+distance) (len-1)],
                                                                                               neighbourY <- [max (y-distance) 0..min (y+distance) (len-1)]],
                                       any (/= Blank) neighbours]

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

instance Show UnboxedVectorBoard where
  show board = L.concat $ drawBoard board
    where
        drawField :: Field -> Char
        drawField field = case field of
                            Player Black -> 'X'
                            Player White -> 'O'
                            Blank -> ' '
        drawLine :: U.Vector Field -> String
        drawLine line = L.intersperse '|' $ fmap (drawField) $ U.toList line
        drawBoard (UnboxedVectorBoard board) = L.intersperse "\n" $ V.toList $ fmap (drawLine) board




exampleBoard :: UnboxedVectorBoard
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

diagonals :: Unbox a => V.Vector (U.Vector a) -> [U.Vector a]
diagonals board =
    diagonals' (V.toList board) []
    where
        diagonals' :: Unbox a => [U.Vector a] -> [U.Vector a] -> [U.Vector a]
        diagonals' [] [] = []
        diagonals' [] openLines = (diagonal openLines) : (diagonals' [] $ L.filter (not . U.null) $ L.map (U.tail) openLines)
        diagonals' (line:lines) openLines =
            (diagonal newOpenLines) :
            (diagonals' lines (openLinesTails))
            where
                newOpenLines = openLines ++ [line]
                openLinesTails =
                    L.filter (not . U.null) $
                    L.map (U.tail) newOpenLines
        diagonal :: Unbox a => [U.Vector a] -> U.Vector a
        diagonal openLines = U.fromList $ L.map (U.head) openLines


allLines :: UnboxedVectorBoard -> [U.Vector Field]
allLines (UnboxedVectorBoard board) =
    V.toList board ++
    (V.toList $ transpose board) ++
    (diagonals board) ++
    (diagonals (V.reverse board))

transpose :: Unbox a => V.Vector (U.Vector a) -> V.Vector (U.Vector a)
transpose matrix = V.generate len (\i -> U.generate len (\n -> matrix V.! n U.! i))
  where len = (V.length matrix)



-- produce all combinations of n elements of value a and m elements of value b
-- ex. combinations 'a' 2 'b' 3 == ["aabbb","ababb","abbab","abbba","baabb","babab","babba","bbaab","bbaba","bbbaa"]

combinations :: a -> Int -> a -> Int -> [[a]]
combinations _ 0 b m = [L.replicate m b]
combinations a n _ 0 = [L.replicate n a]
combinations a n b m =
    [ [a] ++ l | l <- combinations a (n-1) b m ] ++
    [ [b] ++ l | l <- combinations a n b (m-1)]

-- generate list of lists segments of length 'len'
-- ie. subsegments 3 "abcdefg" == ["abc", "bcd", "cde", "def", "efg"]
subsegments :: Unbox a => Int -> U.Vector a -> [U.Vector a]
subsegments len line = [U.slice d len line | d <- [0..((U.length line) - len)]]

-- count open 'Xs' (doubles, threes, fours, fives) for player in given line
openXsInLine :: Int -> Player -> U.Vector Field -> Int
openXsInLine x player line =
  --trace ("prefix patterns:" ++ (concat $ L.intersperse " " $ L.map (showPattern) prefixPatterns)) $
  --trace ("suffix patterns:" ++ (concat $ L.intersperse " " $ L.map (showPattern) suffixPatterns)) $
  --trace ("infix patterns:" ++ (concat $ L.intersperse " " $ L.map (showPattern) infixPatterns)) $
  --trace ("exact patterns:" ++ (concat $ L.intersperse " " $ L.map (showPattern) xsPatternsV)) $
  countMatches
  where
    xsPatterns = combinations Blank (5 - x) (Player player) x
    xsPatternsV = L.map (U.fromList) xsPatterns
    prefixPatterns = L.map (U.fromList) [ pattern ++ [notPlayer] | pattern <- xsPatterns,
                                                notPlayer <- notPlayerField player ]
    suffixPatterns = L.map (U.fromList) [ [notPlayer] ++ pattern | pattern <- xsPatterns,
                                                notPlayer <- notPlayerField player ]
    infixPatterns = L.map (U.fromList) [ [notPlayerFirst] ++ pattern ++ [notPlayerSecond] | pattern <- xsPatterns,
                                                                            notPlayerFirst <- notPlayerField player,
                                                                            notPlayerSecond <- notPlayerField player ]
    matchPrefix pattern = pattern `isPrefixOf` line
    matchSuffix pattern = pattern `isSuffixOf` line
    matchInfix segment pattern = pattern == segment
    matchInfixSegments pattern = fmap (\match -> match pattern) $ fmap (matchInfix) $ subsegments 7 line
    matchExact pattern = pattern == line
    allMatches =
        fmap (matchPrefix) prefixPatterns ++
        fmap (matchSuffix) suffixPatterns ++
        fmap (matchExact) xsPatternsV ++
        (concat $ fmap (matchInfixSegments) infixPatterns)
    countMatches = sum $ fmap (\match -> if match then 1 else 0) allMatches

showPattern :: U.Vector Field -> String
showPattern pattern = concat $ L.map (print) $ U.toList pattern
  where
    print field = case field of
                    Blank -> "_"
                    Player White -> "O"
                    Player Black -> "X"