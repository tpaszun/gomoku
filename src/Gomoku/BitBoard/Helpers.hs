module Gomoku.BitBoard.Helpers where

import Gomoku.BitBoard
import Gomoku.BitBoardImpl()

import qualified Data.Vector.Unboxed as U
import qualified Data.List as L
import Data.Bits
import Data.Word

import Gomoku.Abstractions

showBoardAsBinary :: BitBoard -> String
showBoardAsBinary board =
  "\n board: \n\n" ++ show board ++
  "\n\n horizontal: \n\n" ++ horiz ++
  "\n\n vertical: \n\n" ++ vert ++
  "\n\n left diagonal: \n\n" ++ diagL ++
  "\n\n right diagonal: \n\n" ++ diagR
  where
    (BitBoard _ size) = board
    showBoard b = L.concat $ L.intersperse "\n"
      $ L.zipWith (\idx line -> (showIntPad 2 idx) ++ "    " ++ line) ([0..])
        $ L.map (showLineAsBinary size) $ U.toList b
    horiz = showBoard $ horizontal board
    vert = showBoard $ vertical board
    diagL = showBoard $ diagonalL board
    diagR = showBoard $ diagonalR board

showLineAsBinary :: Int -> Word64 -> String
showLineAsBinary size line = L.concat $ L.intersperse " " $ L.map (showFieldAsBinary) [ shiftR line (i*2) .&. 3 | i <- [size-1,size-2..0] ]

showFieldAsBinary :: Word64 -> String
showFieldAsBinary 0 = "__"
showFieldAsBinary 1 = "01"
showFieldAsBinary 2 = "10"
showFieldAsBinary x = error "Invalid field value: " ++ show x

showIntersection :: U.Vector (Int,Word64) -> String
showIntersection inters =
  L.concat $ L.intersperse "\n" $ L.map (\(len, line) -> showLineAsBinary len line) $ U.toList inters

showIntPad size x =
  leftPad ++ xStr
  where
    xStr = show x
    leftPad = L.replicate (size - (length xStr)) ' '


leftDiagonal :: BitBoard -> Move -> (Int, Word64)
leftDiagonal board move =
    (len, diag)
  where
    diag = U.foldl (\s x ->
            (x .|. s)
            ) 0 $ fields
    (Move x y _) = move
    startX       = if x < y then 0 else x - y
    startY       = if x < y then y - x else 0
    boardSize    = (boardLength board)
    len          = if x < y then boardSize - startY else boardSize - startX
    startLine    = startY
    firstXPos    = startX
    fields       = U.zipWith
                    (\line idx ->
                      shiftR (line .&. shiftL 3 (2*idx) ) (2*startX)

                    )
                    (U.slice startLine len $ horizontal board)
                    (U.enumFromTo startX (startX+len))


rightDiagonal :: BitBoard -> Move -> (Int, Word64)
rightDiagonal board move =
    (len, diag)
  where
    diag = U.foldl (\s x ->
            (x .|. s)
            ) 0 $ fields
    (Move x y _) = move
    boardSize    = (boardLength board)
    startX       = if x + y < (boardSize - 1) then x + y      else boardSize - 1
    startY       = if x + y < (boardSize - 1) then 0          else x + y - boardSize + 1
    len          = if x + y < (boardSize - 1) then startX + 1 else boardSize - startY
    startLine    = startY
    firstXPos    = startX
    fields       = U.zipWith
                    (\line idx ->
                      shiftR (line .&. shiftL 3 (2*idx) ) (2*startY)

                    )
                    (U.slice startLine len $ horizontal board)
                    (U.enumFromStepN startX (-1) (len))