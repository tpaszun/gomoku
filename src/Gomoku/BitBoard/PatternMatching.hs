module Gomoku.BitBoard.PatternMatching (
    openXsInBoard,
    openXsInIntersection
) where

import Gomoku.Abstractions
import Gomoku.Patterns
import Gomoku.BitBoard
import Gomoku.BitBoard.PatternCache

import Data.Word
import Data.Bits
import qualified Data.Vector.Unboxed as U

--------------------
-- Patterns matching
--------------------

-- match prefix pattern to line
matchPrefix :: Word64 -> Int -> Word64 -> Bool
matchPrefix pattern patternLength line =
    ( (line .&. prefixMask) `xor` pattern ) == 0
    where
        prefixMask = 2^(2*patternLength) - 1 -- 4095 -- 2^(2*6) - 1

-- match suffix pattern to line
matchSuffix :: Word64 -> Int -> Int -> Word64 -> Bool
matchSuffix pattern patternLength lineLength line =
  ( (line .&. shiftedMask) `xor` shiftedPattern ) == 0
  where
    shiftedPattern = shiftL pattern ((lineLength - patternLength) * 2)
    shiftedMask = shiftL suffixMask ((lineLength - patternLength) * 2)
    suffixMask = 2^(2*patternLength) - 1

-- match exact pattern to line segment
matchExact :: Word64 -> Word64 -> Bool
matchExact pattern line =
  line `xor` pattern == 0


openXsInBoard :: BitBoard -> Player -> PatternType -> Int
openXsInBoard board =
  openXsInIntersection linesWithLength
  where
    bitBoard = internalRep board
    linesWithLength = U.imap (\index line -> (getLineLength board index, line)) bitBoard

-- get line subsegments of length 7
-- subsegmentsOf7 :: Int -> Word64 -> U.Vector Word64
-- subsegmentsOf7 lineLength line = U.map createSubsegment (U.enumFromTo 0 (lineLength - 7))
--   where
--     mask = 16383 :: Word64 -- (2^(7*2)) - 1     (mask for 7 fields: 11 11 11 11 11 11 11)
--     createSubsegment d = mask .&. (shiftR line (d*2))

subsegmentsOf :: Int -> Int -> Word64 -> U.Vector Word64
subsegmentsOf subsegmentLength lineLength line =
    U.map createSubsegment (U.enumFromTo 0 (lineLength - subsegmentLength))
    where
        mask = (2^(subsegmentLength*2)) - 1 :: Word64 -- 16383  -- (2^(7*2)) - 1     (mask for n fields: 11 11 11 11 11 11 11)
        createSubsegment d = mask .&. (shiftR line (d*2))


openXsInIntersection :: U.Vector (Int, Word64) -> Player -> PatternType -> Int
openXsInIntersection intersection player patternType =
  sumPrefixes + sumSuffixes + sumExacts + sumInfixes
  where
    prefixPatterns = patterns player Prefix patternType
    suffixPatterns = patterns player Suffix patternType
    infixPatterns = patterns player Infix patternType
    exactPatterns = patterns player Exact patternType

    -- All operations done by U.map / U.filter / U.sum, etc to utilize stream fusion

    sumPrefixes = U.sum
      $ U.map (\(_, line) -> U.sum $ U.map (\pattern -> if matchPrefix pattern (patternLength Prefix patternType) line then 1 else 0) prefixPatterns)
      $ U.filter (\(_, line) -> line /= 0)
      $ U.filter (\(len, _) -> len >= patternLength Prefix patternType)
      $ intersection

    sumSuffixes = U.sum
      $ U.map (\(len, line) -> U.sum $ U.map (\pattern -> if matchSuffix pattern (patternLength Suffix patternType) len line then 1 else 0) suffixPatterns)
      $ U.filter (\(_, line) -> line /= 0)
      $ U.filter (\(len, _) -> len >= patternLength Suffix patternType)
      $ intersection

    sumExacts = U.sum
      $ U.map (\(_, line) -> U.sum $ U.map (\pattern -> if matchExact pattern line then 1 else 0) exactPatterns)
      $ U.filter (\(_, line) -> line /= 0)
      $ U.filter (\(len, _) -> len == patternLength Exact patternType)
      $ intersection

    sumInfixes = U.sum
      $ U.map (\(len, line) ->
          U.sum $ U.map (\subsegment -> U.sum $ U.map (\pattern -> if matchExact pattern subsegment then 1 else 0) infixPatterns)
            $ subsegmentsOf (patternLength Infix patternType) len line
        )
      $ U.filter (\(_, line) -> line /= 0)
      $ U.filter (\(len, _) -> len >= patternLength Infix patternType)
      $ intersection