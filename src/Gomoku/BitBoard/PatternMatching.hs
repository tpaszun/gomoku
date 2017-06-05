module Gomoku.BitBoard.PatternMatching (
    openXsInBoard
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

matchPrefix :: Word32 -> Word32 -> Bool
matchPrefix pattern line =
  ( (line .&. prefixMask) `xor` pattern ) == 0

prefixMask :: Word32
prefixMask = 4095 -- 2^(2*6) - 1

matchSuffix :: Word32 -> Int -> Word32 -> Bool
matchSuffix pattern lineLength line =
  ( (line .&. shiftedMask) `xor` shiftedPattern ) == 0
  where
    shiftedPattern = shiftL pattern ((lineLength - 6) * 2)
    shiftedMask = shiftL 4095 ((lineLength - 6) * 2)

matchExact :: Word32 -> Word32 -> Bool
matchExact pattern line =
  line `xor` pattern == 0


openXsInBoard :: BitBoard -> Player -> Int -> Int
openXsInBoard board player x =
  sumPrefixes + sumSuffixes + sumExacts + sumInfixes
  where
    bitBoard = internalRep board
    prefixPatterns = patterns player Prefix x
    suffixPatterns = patterns player Suffix x
    infixPatterns = patterns player Infix x
    exactPatterns = patterns player Exact x

    getLineLen = getLineLength board

    -- All operations done by U.map / U.filter / U.sum, etc to utilize stream fusion

    sumPrefixes = U.sum
      $ U.map (\line -> U.sum $ U.map (\pattern -> if matchPrefix pattern line then 1 else 0) prefixPatterns)
      $ U.filter (\line -> line /= 0)
      $ U.ifilter (\index _ -> (getLineLen index) >= 6) bitBoard

    sumSuffixes = U.sum
      $ U.map (\(len, line) -> U.sum $ U.map (\pattern -> if matchSuffix pattern len line then 1 else 0) suffixPatterns)
      $ U.filter (\(_, line) -> line /= 0)
      $ U.filter (\(len, _) -> len >= 6)
      $ U.imap (\index line -> (getLineLen index, line)) bitBoard

    sumExacts = U.sum
      $ U.map (\(_, line) -> U.sum $ U.map (\pattern -> if matchExact pattern line then 1 else 0) exactPatterns)
      $ U.filter (\(_, line) -> line /= 0)
      $ U.filter (\(len, _) -> len == 5)
      $ U.imap (\index line -> (getLineLen index, line)) bitBoard

    sumInfixes = U.sum
      $ U.map (\(len, line) ->
          U.sum $ U.map (\subsegment -> U.sum $ U.map (\pattern -> if matchExact pattern subsegment then 1 else 0) infixPatterns)
            $ subsegmentsOf7 len line
        )
      $ U.filter (\(_, line) -> line /= 0)
      $ U.filter (\(len, _) -> len >= 7)
      $ U.imap (\index line -> (getLineLen index, line)) bitBoard


-- get line subsegments of length 7
subsegmentsOf7 :: Int -> Word32 -> U.Vector Word32
subsegmentsOf7 lineLength line = U.map createSubsegment (U.enumFromTo 0 (lineLength - 7))
  where
    mask = 16383 :: Word32 -- (2^(7*2)) - 1     (mask for 7 fields: 11 11 11 11 11 11 11)
    createSubsegment d = mask .&. (shiftR line (d*2))

