module Gomoku.Patterns (
  PatternAlign(..),
  PatternType(..),
  genPatterns,
  notPlayerField,
  patternLength,
  combinations,
  printPatternsPos,
  openThreePatterns,
  brokenThreePatterns
) where

import Gomoku.Abstractions

import qualified Data.List as L

data PatternAlign = Prefix | Suffix | Infix | Exact deriving (Show, Enum, Bounded)

data PatternType = Five
                 | StraightFour
                 | Four
                 | Three
                 | OpenThree
                 | BrokenThree
                 | Double
                 deriving (Show, Enum, Bounded)

-- TODO: Add straight four patterns (_XXXX_) with weight equal of five

patternLength :: PatternAlign -> PatternType -> Int
patternLength Infix  StraightFour = 8
patternLength Prefix StraightFour = 7
patternLength Suffix StraightFour = 7
patternLength Exact  StraightFour = 6

patternLength Infix  BrokenThree = 8
patternLength Prefix BrokenThree = 7
patternLength Suffix BrokenThree = 7
patternLength Exact  BrokenThree = 6

patternLength Infix  OpenThree = 9
patternLength Prefix OpenThree = 8
patternLength Suffix OpenThree = 8
patternLength Exact  OpenThree = 7

patternLength Infix  _ = 7
patternLength Prefix _ = 6
patternLength Suffix _ = 6
patternLength Exact  _ = 5

-- generate patterns of x for player and given pattern type
genPatterns :: Player -> PatternAlign -> PatternType -> [[Field]]
genPatterns player patternAlign StraightFour = straightFourPatterns player patternAlign
genPatterns player patternAlign Four = fourPatterns player patternAlign
genPatterns player patternAlign OpenThree = openThreePatterns player patternAlign
genPatterns player patternAlign BrokenThree = brokenThreePatterns player patternAlign

genPatterns player patternAlign pattern = case patternAlign of
    Prefix -> [ pattern ++ [nonPlayer] | pattern <- xsPatterns,
                                         nonPlayer <- notPlayerField player ]
    Suffix -> [ [nonPlayer] ++ pattern | pattern <- xsPatterns,
                                         nonPlayer <- notPlayerField player ]
    Infix -> [ [nonPlayerFirst] ++ pattern ++ [nonPlayerSecond] | pattern <- xsPatterns,
                                                                  nonPlayerFirst <- notPlayerField player,
                                                                  nonPlayerSecond <- notPlayerField player ]
    Exact -> xsPatterns
    where
        x = case pattern of
            Five -> 5
            Three -> 3
            Double -> 2
        xsPatterns = combinations Blank (5 - x) (Player player) x

-- produce all combinations of n elements of value a and m elements of value b
-- ex. combinations 'a' 2 'b' 3 == ["aabbb","ababb","abbab","abbba","baabb","babab","babba","bbaab","bbaba","bbbaa"]
combinations :: a -> Int -> a -> Int -> [[a]]
combinations _ 0 b m = [replicate m b]
combinations a n _ 0 = [replicate n a]
combinations a n b m = [ [a] ++ l | l <- combinations a (n-1) b m ] ++ [ [b] ++ l | l <- combinations a n b (m-1)]

notPlayerField :: Player -> [Field]
notPlayerField player = [Blank, otherPlayerField player] -- for Black: [Blank, Player White]

otherPlayerField :: Player -> Field
otherPlayerField player = case player of
    Black -> Player White
    White -> Player Black


straightFourPatterns :: Player -> PatternAlign -> [[Field]]
straightFourPatterns player patternAlign = case patternAlign of
    Prefix -> [ content ++ [nonPlayer] | nonPlayer <- notPlayerField player]
    Suffix -> [ [nonPlayer] ++ content | nonPlayer <- notPlayerField player]
    Infix -> [ [nonPlayerFirst] ++ content ++ [nonPlayerSecond] | nonPlayerFirst <- notPlayerField player,
                                                                  nonPlayerSecond <- notPlayerField player]
    Exact -> [content]
    where
        content = [Blank] ++ L.replicate 4 (Player player) ++ [Blank]

fourPatterns :: Player -> PatternAlign -> [[Field]]
fourPatterns player patternAlign = case patternAlign of
    Prefix -> [ content ++ [nonPlayer] | content <- xsPatterns,
                                         nonPlayer <- notPlayerField player ]
    Suffix -> [ [nonPlayer] ++ content | content <- xsPatterns,
                                         nonPlayer <- notPlayerField player ]
    Infix -> [ [nonPlayerFirst] ++ content ++ [nonPlayerSecond] | content <- xsPatterns,
                                                                  nonPlayerFirst <- notPlayerField player,
                                                                  nonPlayerSecond <- notPlayerField player,
                                                                  (nonPlayerFirst /= Blank) || (nonPlayerSecond /= Blank) ]
    Exact -> xsPatterns
    where
        xsPatterns = combinations Blank 1 (Player player) 4

brokenThreePatterns :: Player -> PatternAlign -> [[Field]]
brokenThreePatterns player patternAlign = case patternAlign of
    Prefix -> [ content ++ [nonPlayer] | content <- xsPatterns,
                                         nonPlayer <- notPlayerField player ]
    Suffix -> [ [nonPlayer] ++ content | content <- xsPatterns,
                                         nonPlayer <- notPlayerField player ]
    Infix -> [ [nonPlayerFirst] ++ content ++ [nonPlayerSecond] | content <- xsPatterns,
                                                                  nonPlayerFirst <- notPlayerField player,
                                                                  nonPlayerSecond <- notPlayerField player ]
    Exact -> xsPatterns
    where
        xsPatterns = [
          [Blank, Player player, Blank, Player player, Player player, Blank],
          [Blank, Player player, Player player, Blank, Player player, Blank]]

openThreePatterns :: Player -> PatternAlign -> [[Field]]
openThreePatterns player patternAlign = case patternAlign of
    Prefix -> [ xsPattern ++ [nonPlayer] | nonPlayer <- notPlayerField player ]
    Suffix -> [ [nonPlayer] ++ xsPattern | nonPlayer <- notPlayerField player ]
    Infix -> [ [nonPlayerFirst] ++ xsPattern ++ [nonPlayerSecond] | nonPlayerFirst <- notPlayerField player,
                                                                    nonPlayerSecond <- notPlayerField player ]
    Exact -> [xsPattern]
    where
        xsPattern = [Blank, Blank, Player player, Player player, Player player, Blank, Blank]

---------------------------------------------------------
-- Helpers to print positions of patterns in cache vector
---------------------------------------------------------

printPatternsCount =
    L.concat $
    L.intersperse "\n" $
    [ print pattern patternAlign|
        patternAlign <- [(minBound :: PatternAlign)..(maxBound :: PatternAlign)],
        pattern <- [Five, StraightFour, Four, Three, Double] ]
    where
        print pattern patternAlign = (show patternAlign) ++ "\t" ++ (show pattern) ++ "\t\t" ++ (show $ length $ genPatterns Black patternAlign pattern)

printPatternsPos =
    L.concat $
    L.intersperse "\n" $
    zipWith (\a b -> (show $ fst a) ++ " " ++ (show $ snd a) ++ " = " ++ (show b)) plist ilist
    where
        llist = L.map (\(patternAlign, pattern) -> L.length $ genPatterns Black patternAlign pattern) plist
        plist = [ (patternAlign, pattern) |
            patternAlign <- [(minBound :: PatternAlign)..(maxBound :: PatternAlign)],
            pattern <- [Five, StraightFour, Four, Three, OpenThree, BrokenThree, Double] ]
        slist = scanl (\start len -> start + len) 0 llist
        ilist = zipWith (\s p -> (s,p)) slist llist

