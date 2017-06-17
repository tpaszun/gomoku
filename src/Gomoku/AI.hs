module Gomoku.AI where

import Gomoku.Abstractions
import Gomoku.BitBoard
import Gomoku.BitBoardImpl
import qualified Data.List as L
import Data.Tree
import qualified Data.Vector.Unboxed as U

import Data.Tree.Pretty

import Debug.Trace

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

createGameState :: Int -> [Move] -> GameState
createGameState size ms =
    GameState {
        board = b,
        evaluation = evaluateBoard b,
        moves = reverse ms
    }
    where
        b = foldl (updateBoard) (blankBoard size) ms

score :: PlayerEvaluation -> Int
score eval =
    fives eval * 100000 +
    fours eval   * 1000 +
    threes eval   * 100 +
    doubles eval   * 10

totalScore :: GameState -> Int
totalScore (GameState _ eval _) =
    (score $ white $ eval) - (score $ black $ eval)

totalScoreEval :: BoardEvaluation -> Int
totalScoreEval eval =
    (score $ white $ eval) - (score $ black $ eval)

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
    currentPlayer = otherPlayer lastPlayer
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
          nextMoves = genNeighboringMoves updatedBoard 1 (otherPlayer player)

type GameTreeGenerator = GameState -> Tree GameState

minimax :: GameTreeGenerator -> Int -> GameState -> Move
minimax gameTreeGen depth game =
      trace (show zipped) $
      snd $ best
  where
      tree = gameTreeGen game
      nextMoves = fmap (head.moves.rootLabel) $ subForest tree
      (Move _ _ lastPlayer) = (head.moves.rootLabel) tree
      currentPlayer = otherPlayer lastPlayer
      minmaxfn = case currentPlayer of
                   Black -> min'
                   White -> max'
      extrem = case currentPlayer of
                   Black -> minimum
                   White -> maximum
      maxes = fmap (minmaxfn (depth - 1)) $ subForest tree
      zipped = zip maxes nextMoves
      maxVal = fst $ (extrem zipped)
      best = head $ L.filter (\x -> fst x == maxVal) zipped

min' 0 tree =
    if L.null $ subForest tree
        then totalScore $ rootLabel tree
        else minimum $ fmap (totalScore.rootLabel) $ subForest tree
min' n tree =
    if L.null $ subForest tree
        then totalScore $ rootLabel tree
        else minimum $ fmap (max' (n - 1)) $ subForest tree

max' 0 tree =
    if L.null $ subForest tree
        then totalScore $ rootLabel tree
        else maximum $ fmap (totalScore.rootLabel) $ subForest tree
max' n tree =
    if L.null $ subForest tree
        then totalScore $ rootLabel tree
        else maximum $ fmap (min' (n - 1)) $ subForest tree




-- Optimized version

movesTreeInters :: GameTreeGenerator
movesTreeInters game =
    Node {
        rootLabel = game,
        subForest = fmap (nextGameState game) nextMoves
    }
  where
    GameState b bEval ((Move _ _ lastPlayer):_) = game
    currentPlayer = otherPlayer lastPlayer
    nextMoves = genNeighboringMoves b 1 currentPlayer

    nextGameState :: GameState -> Move -> Tree GameState
    nextGameState (GameState _ _ ms) move =
        Node {
            rootLabel = gameState,
            subForest = if gameIsOver newEval
                            then []
                            else fmap (nextGameState gameState) nextMoves
        }
        where
          Move x y player = move
          updatedBoard = updateBoard b move
          oldEval = evaluateIntersection b (x,y)
          newEval = evaluateIntersection updatedBoard (x,y)
          gameState = GameState {
            board = updatedBoard,
            evaluation = add bEval $ dif newEval oldEval,
            moves = move : ms }
          nextMoves = genNeighboringMoves updatedBoard 1 (otherPlayer player)


movesTreeOnlyBest :: Int -> GameTreeGenerator
movesTreeOnlyBest numBest game =
    Node {
        rootLabel = game,
        subForest = L.map (nextGameState game)  bestMoves
    }
  where
    GameState b bEval ((Move _ _ lastPlayer):_) = game
    currentPlayer = otherPlayer lastPlayer

    bestMoves =
        trace ("moves with score: " ++ (show sortedMovesWithScore)) $
        L.take numBest $ L.map (snd) sortedMovesWithScore
      where
        allNextMoves = genNeighboringMoves b 2 currentPlayer
        movesWithScore = L.map (\m -> (evalMove m, m)) allNextMoves
        sortedMovesWithScore = L.reverse $ L.sort movesWithScore
        evalMove move@(Move x y _) =
            (score $ (playerScore currentPlayer) $ evaluateIntersectionForMove b move) +
            (score $ (playerScore lastPlayer) $ evaluateIntersectionForMove b enemyMove)
            where
              enemyMove = Move x y lastPlayer

    nextGameState :: GameState -> Move -> Tree GameState
    nextGameState (GameState _ _ ms) move =
        Node {
            rootLabel = gameState,
            subForest = if gameIsOver newEval
                            then []
                            else fmap (nextGameState gameState) bestMoves
        }
        where
          Move x y player = move
          currentPlayer = otherPlayer player
          updatedBoard = updateBoard b move
          oldEval = evaluateIntersection b (x,y)
          newEval = evaluateIntersection updatedBoard (x,y)
          gameState = GameState {
            board = updatedBoard,
            evaluation = add bEval $ dif newEval oldEval,
            moves = move : ms }
          bestMoves =
              L.take numBest $ L.map (snd) sortedMovesWithScore
            where
              allNextMoves = genNeighboringMoves updatedBoard 2 (currentPlayer)
              movesWithScore = L.map (\m -> (evalMove m, m)) allNextMoves
              sortedMovesWithScore = L.reverse $ L.sort movesWithScore
              evalMove move@(Move x y _) =
                (score $ (playerScore currentPlayer) $ evaluateIntersectionForMove updatedBoard move) +
                (score $ (playerScore player) $ evaluateIntersectionForMove updatedBoard enemyMove)
                where
                  enemyMove = Move x y player

playerScore p = case p of
                  Black -> black
                  White -> white

printMovesTree :: Tree GameState -> Tree String
printMovesTree tree =
    Node {
        rootLabel = (show (x, y)) ++ " " ++ (show $ totalScore node),
        subForest = L.map (printMovesTree) $ subForest tree
    }
    where
      node = rootLabel tree
      (Move x y _) = head $ moves node

