module Gomoku.BitBoard.Wizardry where

import Data.Word
import Data.Bits
import Data.Char
import Numeric

import Gomoku.Abstractions


----------------
-- Bits wizardry
----------------

lineElem :: Word64 -> Int -> Field
lineElem line n =
  intToField intField
    where
      intField = (shiftR line (n*2)) .&. 3

lineToFieldList :: Int -> Word64 -> [Field]
lineToFieldList 0 _ = []
lineToFieldList len line = intToField intField : (lineToFieldList (len-1) $ shiftR line 2)
    where
      intField = line .&. 3

intToField :: Word64 -> Field
intToField 0 = Blank
intToField 1 = Player Black
intToField 2 = Player White
intToField x = error $ "invalid field value: " ++ (show x)

setLineElem :: Word64 -> Int -> Field -> Word64
setLineElem line n field =
  line .|. intField
  where
    intField = shiftL (fromField field) (n*2)

fromLine :: [Field] -> Word64
fromLine line = fromLineAcc line 0 0
  where
    fromLineAcc [] _ acc = acc
    fromLineAcc (x:xs) n acc = fromLineAcc xs (n+1) newAcc
      where newAcc = ( (shift (fromField x) (n*2) ) .|. acc )

fromField :: Field -> Word64
fromField Blank = 0
fromField (Player Black) = 1
fromField (Player White) = 2



showAsBinary :: (Show a, Integral a) => a -> String
showAsBinary x = showIntAtBase 2 intToDigit x ""
