module Gomoku.Abstractions where

import Data.List

data Player = Black
            | White
            deriving (Eq, Ord)

data Field = Blank
           | Player Player
           deriving (Eq)

data Move = Move Int Int Player deriving (Show, Eq, Ord)

class Board a where
    blankBoard :: Int ->  a
    genNeighboringMoves :: a -> Int -> Player -> [Move]
    updateBoard :: a -> Move -> a
    evaluateBoard :: a -> BoardEvaluation
    evaluatePlayer :: a -> Player -> PlayerEvaluation

data BoardEvaluation = BoardEvaluation {
    black :: PlayerEvaluation,
    white :: PlayerEvaluation
}

data PlayerEvaluation = PlayerEvaluation {
    fives :: Int,
    straightFours :: Int,
    fours :: Int,
    threes :: Int,
    openThrees :: Int,
    brokenThrees :: Int,
    doubles :: Int
}

add :: BoardEvaluation -> BoardEvaluation -> BoardEvaluation
add be1 be2 =
  BoardEvaluation {
      black = PlayerEvaluation {
          fives = (fives $ black be1) + (fives $ black be2),
          straightFours = (straightFours $ black be1) + (straightFours $ black be2),
          fours = (fours $ black be1) + (fours $ black be2),
          threes = (threes $ black be1) + (threes $ black be2),
          openThrees = (openThrees $ black be1) + (openThrees $ black be2),
          brokenThrees = (brokenThrees $ black be1) + (brokenThrees $ black be2),
          doubles = (doubles $ black be1) + (doubles $ black be2)
      },
      white = PlayerEvaluation {
          fives = (fives $ white be1) + (fives $ white be2),
          straightFours = (straightFours $ white be1) + (straightFours $ white be2),
          fours = (fours $ white be1) + (fours $ white be2),
          threes = (threes $ white be1) + (threes $ white be2),
          openThrees = (openThrees $ white be1) + (openThrees $ white be2),
          brokenThrees = (brokenThrees $ white be1) + (brokenThrees $ white be2),
          doubles = (doubles $ white be1) + (doubles $ white be2)
      }
  }

dif :: BoardEvaluation -> BoardEvaluation -> BoardEvaluation
dif be1 be2 =
  BoardEvaluation {
      black = PlayerEvaluation {
          fives = (fives $ black be1) - (fives $ black be2),
          straightFours = (straightFours $ black be1) - (straightFours $ black be2),
          fours = (fours $ black be1) - (fours $ black be2),
          threes = (threes $ black be1) - (threes $ black be2),
          openThrees = (openThrees $ black be1) - (openThrees $ black be2),
          brokenThrees = (brokenThrees $ black be1) - (brokenThrees $ black be2),
          doubles = (doubles $ black be1) - (doubles $ black be2)
      },
      white = PlayerEvaluation {
          fives = (fives $ white be1) - (fives $ white be2),
          straightFours = (straightFours $ white be1) - (straightFours $ white be2),
          fours = (fours $ white be1) - (fours $ white be2),
          threes = (threes $ white be1) - (threes $ white be2),
          openThrees = (openThrees $ white be1) - (openThrees $ white be2),
          brokenThrees = (brokenThrees $ white be1) - (brokenThrees $ white be2),
          doubles = (doubles $ white be1) - (doubles $ white be2)
      }
  }

otherPlayer :: Player -> Player
otherPlayer player = case player of
                       Black -> White
                       White -> Black

instance Show Player where
    show Black = "X"
    show White = "O"

instance Show Field where
    show (Player p) = show p
    show Blank = "_"

instance Show PlayerEvaluation where
    show eval =
        concat $ intersperse " " rendered
        where
            values = fmap (\f -> f eval) [fives, straightFours, fours, threes, openThrees, brokenThrees, doubles]
            labels = ["fives", "straight fours", "fours", "threes", "open threes", "broken threes", "doubles"]
            zipped = zip labels values
            filtered = filter (\(_,value) -> value /= 0) zipped
            rendered = fmap (\(label, value) -> label ++ ": " ++ show value) filtered

instance Show BoardEvaluation where
    show eval =
        "Evaluation: X { " ++ (show $ black eval) ++ " } O { " ++ (show $ white eval) ++ " }"