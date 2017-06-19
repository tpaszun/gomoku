module Gomoku.GameState where

import Gomoku.Abstractions
import Gomoku.BitBoard
import Gomoku.BitBoardImpl()

data GameState = GameState {
    board :: BitBoard,
    evaluation :: BoardEvaluation,
    moves :: [Move]
}

instance Show GameState where
    show game =
        --(show $ score $ evaluation game) ++ " " ++
        (show $ head $ moves game) ++ " " ++ (show $ evaluation game)


newGame :: GameState
newGame = GameState {
    board = newBoard,
    evaluation = BoardEvaluation {
        black = emptyEvaluation,
        white = emptyEvaluation
    },
    moves = []
}
  where
    newBoard = blankBoard 15
    emptyEvaluation = PlayerEvaluation {
        fives = 0,
        straightFours = 0,
        fours = 0,
        threes = 0,
        openThrees = 0,
        brokenThrees = 0,
        doubles = 0
    }


updateGameState :: GameState -> Move -> GameState
updateGameState gameState move =
    GameState {
      board = updatedBoard,
      evaluation = evaluateBoard updatedBoard,
      moves = move : moveHistory
    }
  where
    GameState bitboard _ moveHistory = gameState
    updatedBoard = updateBoard bitboard move

createGameState :: Int -> [Move] -> GameState
createGameState size ms =
    GameState {
        board = b,
        evaluation = evaluateBoard b,
        moves = reverse ms
    }
    where
        b = foldl (updateBoard) (blankBoard size) ms