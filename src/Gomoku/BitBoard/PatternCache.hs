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
patterns :: Player -> PatternType -> Pattern -> U.Vector Word64
patterns player patternType pattern =
  U.slice s l patternsCache
  where
    (s, l) = patternsRange player patternType pattern


-- index and length selector for patterns of given player, type and x
patternsRange :: Player -> PatternType -> Pattern -> (Int, Int)
patternsRange Black Prefix Five = (0,2)
patternsRange Black Prefix StraightFour = (2,2)
patternsRange Black Prefix Four = (4,10)
patternsRange Black Prefix Three = (14,20)
patternsRange Black Prefix Double = (34,20)

patternsRange Black Suffix Five = (54,2)
patternsRange Black Suffix StraightFour = (56,2)
patternsRange Black Suffix Four = (58,10)
patternsRange Black Suffix Three = (68,20)
patternsRange Black Suffix Double = (88,20)

patternsRange Black Infix Five = (108,4)
patternsRange Black Infix StraightFour = (112,4)
patternsRange Black Infix Four = (116,15)
patternsRange Black Infix Three = (131,40)
patternsRange Black Infix Double = (171,40)

patternsRange Black Exact Five = (211,1)
patternsRange Black Exact StraightFour = (212,1)
patternsRange Black Exact Four = (213,5)
patternsRange Black Exact Three = (218,10)
patternsRange Black Exact Double = (228,10)

patternsRange White patternType x = (238 + bx, by)
  where (bx, by) = patternsRange Black patternType x

patternsRange _ _ _ = error $ "invalid patternsRange request"


-- store all generated patterns as a unboxed vector of patterns
patternsCache :: U.Vector Word64
patternsCache =
  U.fromList $ L.map (fromLine) $
  L.concat [ genPatterns player patternType pattern | player <- [Black, White],
                                                      patternType <- [(minBound :: PatternType)..(maxBound :: PatternType)],
                                                      pattern <- [Five, StraightFour, Four, Three, Double]]
