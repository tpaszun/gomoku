module Gomoku.AI where

import Gomoku.Abstractions
import Gomoku.BitBoard
import Gomoku.BitBoardImpl
import Gomoku.GameState
import qualified Data.List as L
import Data.Tree
import qualified Data.Vector.Unboxed as U

import Data.Tree.Pretty

import Debug.Trace


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
    ((fives $ black eval) > 0)
    ||
    ((fives $ white eval) > 0)

----------------------
-- Generate moves tree
----------------------

type GameTreeGenerator = GameState -> Tree GameState

minimax :: GameTreeGenerator -> Int -> GameState -> Move
minimax gameTreeGen depth game =
    --   trace (show zipped) $
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


movesTreeOnlyBest :: Int -> GameTreeGenerator
movesTreeOnlyBest numBest game =
    Node {
        rootLabel = game,
        subForest = if gameIsOver $ evaluation game
            then []
            else L.map (nextGameState) nextMoves
    }
    where
        GameState b bEval ((Move _ _ lastPlayer):_) = game
        currentPlayer = otherPlayer lastPlayer

        nextMoves =
            if not $ null winningMove
            then winningMove
            else
                if not $ null nonLosingMove
                then nonLosingMove
                else bestMoves

        winningMove = take 1 (winningMoves game currentPlayer)
        nonLosingMove = take 1 (nonLosingMoves game currentPlayer)

        bestMoves = L.take numBest $ L.map (snd) sortedMovesWithScore
            where
                allNextMoves = genNeighboringMoves b 2 currentPlayer
                movesWithScore = L.map (\m -> (evalMove m, m)) allNextMoves
                sortedMovesWithScore = L.reverse $ L.sort movesWithScore
                evalMove move@(Move x y _) =
                    (score $ (playerScore currentPlayer) $ evaluateIntersectionForMove b move) +
                    (score $ (playerScore lastPlayer) $ evaluateIntersectionForMove b enemyMove)
                    where
                        enemyMove = Move x y lastPlayer

        nextGameState :: Move -> Tree GameState
        nextGameState move =
            movesTreeOnlyBest numBest gameState
            where
                Move x y player = move
                currentPlayer = otherPlayer player
                updatedBoard = updateBoard b move
                oldEval = evaluateIntersection b (x,y)
                newEval = evaluateIntersection updatedBoard (x,y)
                gameState = GameState {
                    board = updatedBoard,
                    evaluation = add bEval $ dif newEval oldEval,
                    moves = move : moves game }

playerScore p = case p of
                  Black -> black
                  White -> white

winningMoves :: GameState -> Player -> [Move]
winningMoves gameState player =
    filter (\move -> (fives $ playerEval $ evaluateIntersectionForMove board move) > 0) neighbouringMoves
    where
        (GameState board _ _) = gameState
        playerEval = case player of
            Black -> black
            White -> white
        neighbouringMoves = genNeighboringMoves board 1 player

nonLosingMoves :: GameState -> Player -> [Move]
nonLosingMoves gameState player =
    map (\(Move x y _) -> Move x y player) $ winningMoves gameState $ otherPlayer player