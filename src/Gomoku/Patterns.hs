module Gomoku.Patterns where

import Gomoku.Abstractions

data PatternType = Prefix | Suffix | Infix | Exact deriving (Show, Enum, Bounded)

-- generate patterns of x for player and given pattern type
genPatterns :: Player -> PatternType -> Int -> [[Field]]
genPatterns player patternType x =
  case patternType of
    Prefix -> [ pattern ++ [nonPlayer] | pattern <- xsPatterns,
                                         nonPlayer <- notPlayerField player ]
    Suffix -> [ [nonPlayer] ++ pattern | pattern <- xsPatterns,
                                         nonPlayer <- notPlayerField player ]
    Infix -> [ [nonPlayerFirst] ++ pattern ++ [nonPlayerSecond] | pattern <- xsPatterns,
                                                                  nonPlayerFirst <- notPlayerField player,
                                                                  nonPlayerSecond <- notPlayerField player ]
    Exact -> xsPatterns
  where
    xsPatterns = combinations Blank (5 - x) (Player player) x


-- produce all combinations of n elements of value a and m elements of value b
-- ex. combinations 'a' 2 'b' 3 == ["aabbb","ababb","abbab","abbba","baabb","babab","babba","bbaab","bbaba","bbbaa"]
combinations :: a -> Int -> a -> Int -> [[a]]
combinations _ 0 b m = [replicate m b]
combinations a n _ 0 = [replicate n a]
combinations a n b m = [ [a] ++ l | l <- combinations a (n-1) b m ] ++ [ [b] ++ l | l <- combinations a n b (m-1)]