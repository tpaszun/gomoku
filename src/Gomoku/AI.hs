module Gomoku.AI where

import Gomoku.Abstractions
--import Gomoku.ListImpl
--import Gomoku.UnboxedVectorImpl
import Gomoku.BitBoard
import Gomoku.BitBoardImpl()
import Data.List()
import Data.Tree
import qualified Data.Vector.Unboxed as U

import Debug.Trace

data GameState = GameState {
    --board :: ListBoard,
    --board :: UnboxedVectorBoard,
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
    newBoard = blankBoard 7
    emptyEvaluation = PlayerEvaluation {
        fives = 0,
        fours = 0,
        threes = 0,
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


score :: PlayerEvaluation -> Int
score eval =
    fives eval * 10000 +
    fours eval * 1000 +
    threes eval * 100 +
    doubles eval * 10

gameIsOver :: BoardEvaluation -> Bool
gameIsOver eval =
    (fives $ black eval) > 0 ||
    (fives $ white eval) > 0

----------------------
-- Generate moves tree
----------------------


movesTree :: GameState -> Tree GameState
movesTree game =
    Node {
        rootLabel = game,
        subForest = fmap (nextGameState game) nextMoves
    }
  where
    GameState board _ ((Move _ _ lastPlayer):_) = game
    currentPlayer = other lastPlayer
    nextMoves = genNeighboringMoves board 1 currentPlayer
    nextGameState :: GameState -> Move -> Tree GameState
    nextGameState (GameState board _ moves) move =
        Node {
            rootLabel = gameState,
            subForest = --if gameIsOver $ evaluation gameState
                        --then []
                        --else
                        fmap (nextGameState gameState) nextMoves
        }
        where
          Move _ _ player = move
          updatedBoard = updateBoard board move
          gameState = GameState {
            board = updatedBoard,
            evaluation = (evaluateBoard updatedBoard),
            moves = move : moves }
          nextMoves = genNeighboringMoves updatedBoard 1 (other player)



minimax :: Int -> GameState -> Move
minimax depth game =
  --if not $ null winningMoves
  --then head winningMoves
  --else
      --trace (show zipped) $
      snd $ maximum zipped
  where
      tree = movesTree game
      --winningSubtrees = filter (gameIsOver.evaluation) $ fmap (rootLabel) $ subForest tree
      --winningMoves = fmap (head.moves) winningSubtrees
      nextMoves = fmap (head.moves.rootLabel) $ subForest tree
      maxes = fmap (min' (depth - 1)) $ subForest tree
      zipped = zip maxes nextMoves

min' 0 tree =
    minimum $ fmap (totalScore.rootLabel) $ subForest tree
min' n tree = minimum $ fmap (max' (n - 1)) $ subForest tree

max' 0 tree = maximum $ fmap (totalScore.rootLabel) $ subForest tree
max' n tree =
    maximum $ fmap (min' (n - 1)) $ subForest tree

totalScore :: GameState -> Int
totalScore (GameState _ eval _) =
    --trace ("totalScore: " ++ show t) $
    t
    where t = (score $ white $ eval) - (score $ black $ eval)
