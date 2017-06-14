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
patterns :: Player -> PatternType -> Int -> U.Vector Word64
patterns player patternType x =
  U.slice s l patternsCache
  where
    (s, l) = patternsRange player patternType x


-- index and length selector for patterns of given player, type and x
patternsRange :: Player -> PatternType -> Int -> (Int, Int)
patternsRange Black Prefix 5 = (0, 2)
patternsRange Black Prefix 4 = (2, 10)
patternsRange Black Prefix 3 = (12, 20)
patternsRange Black Prefix 2 = (32, 20)

patternsRange Black Suffix 5 = (52, 2)
patternsRange Black Suffix 4 = (54, 10)
patternsRange Black Suffix 3 = (64, 20)
patternsRange Black Suffix 2 = (84, 20)

patternsRange Black Infix 5 = (104, 4)
patternsRange Black Infix 4 = (108, 20)
patternsRange Black Infix 3 = (128, 40)
patternsRange Black Infix 2 = (168, 40)

patternsRange Black Exact 5 = (208, 1)
patternsRange Black Exact 4 = (209, 5)
patternsRange Black Exact 3 = (214, 10)
patternsRange Black Exact 2 = (224, 10)

patternsRange White patternType x = (234 + bx, by)
  where (bx, by) = patternsRange Black patternType x

patternsRange _ _ _ = error $ "invalid patternsRange request"


-- store all generated patterns as a unboxed vector of patterns
patternsCache :: U.Vector Word64
patternsCache =
  U.fromList $ L.map (fromLine) $
  L.concat [ genPatterns player patternType len | player <- [Black, White],
                                                  patternType <- [(minBound :: PatternType)..(maxBound :: PatternType)],
                                                  len <- [5,4..2]]

