module Gomoku.Patterns (
  PatternType(..),
  Pattern(..),
  genPatterns,
  notPlayerField,
  patternLength,
  combinations
) where

import Gomoku.Abstractions

import qualified Data.List as L

data PatternType = Prefix | Suffix | Infix | Exact deriving (Show, Enum, Bounded)

data Pattern = Five
             | StraightFour
             | Four
             | Three
             | BrokenThree
             | Double
             deriving (Show, Enum, Bounded)

-- TODO: Add straight four patterns (_XXXX_) with weight equal of five

patternLength :: PatternType -> Pattern -> Int
patternLength Infix  StraightFour = 8
patternLength Prefix StraightFour = 7
patternLength Suffix StraightFour = 7
patternLength Exact  StraightFour = 6

patternLength Infix  _ = 7
patternLength Prefix _ = 6
patternLength Suffix _ = 6
patternLength Exact  _ = 5

-- generate patterns of x for player and given pattern type
genPatterns :: Player -> PatternType -> Pattern -> [[Field]]
genPatterns player patternType StraightFour = straightFoursPatterns player patternType
genPatterns player patternType Four = foursPatterns player patternType

genPatterns player patternType pattern = case patternType of
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


straightFoursPatterns :: Player -> PatternType -> [[Field]]
straightFoursPatterns player patternType = case patternType of
    Prefix -> [ content ++ [nonPlayer] | nonPlayer <- notPlayerField player]
    Suffix -> [ [nonPlayer] ++ content | nonPlayer <- notPlayerField player]
    Infix -> [ [nonPlayerFirst] ++ content ++ [nonPlayerSecond] | nonPlayerFirst <- notPlayerField player,
                                                                  nonPlayerSecond <- notPlayerField player]
    Exact -> [content]
    where
        content = [Blank] ++ L.replicate 4 (Player player) ++ [Blank]

foursPatterns :: Player -> PatternType -> [[Field]]
foursPatterns player patternType = case patternType of
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