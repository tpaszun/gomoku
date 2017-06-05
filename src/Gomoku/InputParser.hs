module Gomoku.InputParser (
    parseMove
) where

import Gomoku.Abstractions

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Number
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Error

import Data.Char

--------------
-- Move parser
--------------

parsePosI :: Parser Int
parsePosI = do
            x <- int
            if (x < 1 || x > 19) then
              unexpected "Tylko liczby od 1 - 19"
            else
              return x

parsePosC :: Parser Int
parsePosC = do
            x <- lower
            if (x < 'a' || x > 'z') then
              unexpected "Tylko znaki od a - z"
            else
              return $ (ord x) - (ord 'a') + 1


parseSinglePos = choice [parsePosI, parsePosC]

parsePos = do
              x <- parseSinglePos
              spaces
              y <- parseSinglePos
              return (x,y)

parseMove :: Player -> String -> Move
parseMove player input =
  case parse parsePos "Parse error" input of
    Right (x, y) -> Move x y player
    Left x -> error (show x)