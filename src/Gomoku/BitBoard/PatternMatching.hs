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

-- prefix and suffix patterns are 6 fields long (6 * 2 bits = 12 bits)

matchPrefix :: Word64 -> Word64 -> Bool
matchPrefix pattern line =
  ( (line .&. prefixMask) `xor` pattern ) == 0

prefixMask :: Word64
prefixMask = 4095 -- 2^(2*6) - 1

matchSuffix :: Word64 -> Int -> Word64 -> Bool
matchSuffix pattern lineLength line =
  ( (line .&. shiftedMask) `xor` shiftedPattern ) == 0
  where
    shiftedPattern = shiftL pattern ((lineLength - 6) * 2)
    shiftedMask = shiftL 4095 ((lineLength - 6) * 2)

matchExact :: Word64 -> Word64 -> Bool
matchExact pattern line =
  line `xor` pattern == 0


openXsInBoard :: BitBoard -> Player -> Pattern -> Int
openXsInBoard board =
  openXsInIntersection linesWithLength
  where
    bitBoard = internalRep board
    linesWithLength = U.imap (\index line -> (getLineLength board index, line)) bitBoard

-- get line subsegments of length 7
subsegmentsOf7 :: Int -> Word64 -> U.Vector Word64
subsegmentsOf7 lineLength line = U.map createSubsegment (U.enumFromTo 0 (lineLength - 7))
  where
    mask = 16383 :: Word64 -- (2^(7*2)) - 1     (mask for 7 fields: 11 11 11 11 11 11 11)
    createSubsegment d = mask .&. (shiftR line (d*2))


openXsInIntersection :: U.Vector (Int, Word64) -> Player -> Pattern -> Int
openXsInIntersection intersection player pattern =
  sumPrefixes + sumSuffixes + sumExacts + sumInfixes
  where
    prefixPatterns = patterns player Prefix pattern
    suffixPatterns = patterns player Suffix pattern
    infixPatterns = patterns player Infix pattern
    exactPatterns = patterns player Exact pattern

    -- All operations done by U.map / U.filter / U.sum, etc to utilize stream fusion

    sumPrefixes = U.sum
      $ U.map (\(_, line) -> U.sum $ U.map (\pattern -> if matchPrefix pattern line then 1 else 0) prefixPatterns)
      $ U.filter (\(_, line) -> line /= 0)
      $ U.filter (\(len, _) -> len >= patternLength Prefix pattern)
      $ intersection

    sumSuffixes = U.sum
      $ U.map (\(len, line) -> U.sum $ U.map (\pattern -> if matchSuffix pattern len line then 1 else 0) suffixPatterns)
      $ U.filter (\(_, line) -> line /= 0)
      $ U.filter (\(len, _) -> len >= patternLength Suffix pattern)
      $ intersection

    sumExacts = U.sum
      $ U.map (\(_, line) -> U.sum $ U.map (\pattern -> if matchExact pattern line then 1 else 0) exactPatterns)
      $ U.filter (\(_, line) -> line /= 0)
      $ U.filter (\(len, _) -> len == patternLength Exact pattern)
      $ intersection

    sumInfixes = U.sum
      $ U.map (\(len, line) ->
          U.sum $ U.map (\subsegment -> U.sum $ U.map (\pattern -> if matchExact pattern subsegment then 1 else 0) infixPatterns)
            $ subsegmentsOf7 len line
        )
      $ U.filter (\(_, line) -> line /= 0)
      $ U.filter (\(len, _) -> len >= patternLength Infix pattern)
      $ intersection