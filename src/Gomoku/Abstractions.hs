{-# LANGUAGE ExistentialQuantification #-}

module Gomoku.Abstractions where

import Data.List

data Player = Black
            | White
            deriving (Eq, Ord)

instance Show Player where
    show Black = "X"
    show White = "O"

data Field = Blank
           | Player Player
           deriving (Eq)

instance Show Field where
    show (Player p) = show p
    show Blank = "_"

data Move = Move Int Int Player deriving (Show, Eq, Ord)

{-
instance Show Move where
    show (Move x y player) = "(" ++ show player ++ " " ++ show x ++ " " ++ show y ++ ")"
-}

class Board a where
    blankBoard :: Int ->  a
    genMoves :: a -> Player -> [Move]
    genNeighboringMoves :: a -> Int -> Player -> [Move]
    updateBoard :: a -> Move -> a
    evaluateBoard :: a -> BoardEvaluation
    evaluatePlayer :: a -> Player -> PlayerEvaluation


data PlayerEvaluation = PlayerEvaluation {
    fives :: Int,
    fours :: Int,
    threes :: Int,
    doubles :: Int
}

instance Show PlayerEvaluation where
    show eval =
        concat $ intersperse " " rendered
        where
            values = fmap (\f -> f eval) [fives, fours, threes, doubles]
            labels = ["fives", "fours", "threes", "doubles"]
            zipped = zip labels values
            filtered = filter (\(_,value) -> value > 0) zipped
            rendered = fmap (\(label, value) -> label ++ ": " ++ show value) filtered

data BoardEvaluation = BoardEvaluation {
    black :: PlayerEvaluation,
    white :: PlayerEvaluation
}

instance Show BoardEvaluation where
    show eval =
        "Evaluation: X { " ++ (show $ black eval) ++ " } O { " ++ (show $ white eval) ++ " }"

other :: Player -> Player
other player = case player of
                       Black -> White
                       White -> Black

otherPlayer :: Player -> Field
otherPlayer player = case player of
                       Black -> Player White
                       White -> Player Black

notPlayerField :: Player -> [Field]
notPlayerField player = [Blank, otherPlayer player] -- for Black: [Blank, Player White]


data GameState' = forall b. Board b => GameState' {
    board' :: b,
    evaluation' :: BoardEvaluation,
    moves' :: [Move]
}

instance Show GameState' where
    show game =
        --(show $ score $ evaluation game) ++ " " ++
        (show $ head $ moves' game) ++ " " ++ (show $ evaluation' game)
