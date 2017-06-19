module Gomoku.BitBoard.PatternCache (
  patterns
) where

import Data.Word
import qualified Data.Vector.Unboxed as U
import qualified Data.List as L

import Gomoku.Abstractions
import Gomoku.Patterns
import Gomoku.BitBoard.Wizardry


-- get patterns from patterns cache
patterns :: Player -> PatternAlign -> PatternType -> U.Vector Word64
patterns player patternAlign patternType =
  U.slice s l patternsCache
  where
    (s, l) = patternsRange player patternAlign patternType


-- index and length selector for patterns of given player, type and x
patternsRange :: Player -> PatternAlign -> PatternType -> (Int, Int)

patternsRange Black Prefix Five = (0,2)
patternsRange Black Prefix StraightFour = (2,2)
patternsRange Black Prefix Four = (4,10)
patternsRange Black Prefix Three = (14,20)
patternsRange Black Prefix OpenThree = (34,2)
patternsRange Black Prefix BrokenThree = (36,4)
patternsRange Black Prefix Double = (40,20)

patternsRange Black Suffix Five = (60,2)
patternsRange Black Suffix StraightFour = (62,2)
patternsRange Black Suffix Four = (64,10)
patternsRange Black Suffix Three = (74,20)
patternsRange Black Suffix OpenThree = (94,2)
patternsRange Black Suffix BrokenThree = (96,4)
patternsRange Black Suffix Double = (100,20)

patternsRange Black Infix Five = (120,4)
patternsRange Black Infix StraightFour = (124,4)
patternsRange Black Infix Four = (128,15)
patternsRange Black Infix Three = (143,40)
patternsRange Black Infix OpenThree = (183,4)
patternsRange Black Infix BrokenThree = (187,8)
patternsRange Black Infix Double = (195,40)

patternsRange Black Exact Five = (235,1)
patternsRange Black Exact StraightFour = (236,1)
patternsRange Black Exact Four = (237,5)
patternsRange Black Exact Three = (242,10)
patternsRange Black Exact OpenThree = (252,1)
patternsRange Black Exact BrokenThree = (253,2)
patternsRange Black Exact Double = (255,10)

patternsRange White patternAlign x = (265 + bx, by)
  where (bx, by) = patternsRange Black patternAlign x

patternsRange _ _ _ = error $ "invalid patternsRange request"


-- store all generated patterns as a unboxed vector of patterns
patternsCache :: U.Vector Word64
patternsCache =
  U.fromList $ L.map (fromLine) $
  L.concat [ genPatterns player patternAlign pattern | player <- [Black, White],
                                                      patternAlign <- [(minBound :: PatternAlign)..(maxBound :: PatternAlign)],
                                                      pattern <- [Five, StraightFour, Four, Three, OpenThree, BrokenThree, Double]]
