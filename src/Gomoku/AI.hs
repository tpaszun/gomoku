module Gomoku.AI where

import Gomoku.Abstractions
import Gomoku.BitBoard
import Gomoku.BitBoardImpl
import Gomoku.GameState
import Gomoku.Utils
import qualified Data.List as L
import Data.Tree
import qualified Data.Vector.Unboxed as U

import Data.Tree.Pretty
import Data.Maybe

import Debug.Trace


score :: PlayerEvaluation -> Int
score eval =
    fives eval         * 100000 +
    straightFours eval  * 10000 +
    fours eval           * 1000 +
    threes eval           * 100 +
    doubles eval           * 10

totalScore :: GameState -> Int
totalScore (GameState _ eval _) =
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
    -- trace ("Candidates: " ++ (show $ bestMoves (board game) currentPlayer 10)) $
    -- trace ("Best moves: " ++ (show zipped)) $
    snd $ bestMove
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
        bestMove = head $ L.filter (\x -> fst x == maxVal) zipped

min' 0 tree =
    if L.null $ subForest tree
        then totalScore $ rootLabel tree
        else minimum $ fmap (totalScore.rootLabel) $ subForest tree
min' n tree =
    case nextMovesCount of
        0 -> totalScore $ rootLabel tree
        1 -> minimum $ fmap (max' (n)) $ subForest tree
        _ -> minimum $ fmap (max' (n - 1)) $ subForest tree
    where
        nextMovesCount = L.length $ subForest tree
        move = head $ moves $ rootLabel tree

max' 0 tree =
    if L.null $ subForest tree
        then totalScore $ rootLabel tree
        else maximum $ fmap (totalScore.rootLabel) $ subForest tree
max' n tree =
    case nextMovesCount of
        0 -> totalScore $ rootLabel tree
        1 -> maximum $ fmap (min' (n)) $ subForest tree
        _ -> maximum $ fmap (min' (n - 1)) $ subForest tree
    where
        nextMovesCount = L.length $ subForest tree
        move = head $ moves $ rootLabel tree

movesTreeOnlyBest :: Int -> GameTreeGenerator
movesTreeOnlyBest numBest game =
    Node {
        rootLabel = game,
        subForest = if gameIsOver $ evaluation game
            then []
            else L.map (nextGameState) nextMoves
    }
    where
        GameState currentBoard bEval ((Move _ _ lastPlayer):_) = game
        currentPlayer = otherPlayer lastPlayer

        nextMoves = case listToMaybe (winningMoves game currentPlayer) of
            Just m -> [m]
            Nothing -> case listToMaybe (nonLosingMoves game currentPlayer) of
                Just m -> [m]
                Nothing -> case listToMaybe (winningInNextTurnMoves game currentPlayer) of
                    Just m -> [m]
                    Nothing -> nonLosingMovesAndBest

        nonLosingMovesAndBest = rmdups $ (best) ++ (nonLosingInNextTurnMoves game currentPlayer)

        best = L.map (snd) $ bestMoves currentBoard currentPlayer numBest

        nextGameState :: Move -> Tree GameState
        nextGameState move =
            movesTreeOnlyBest numBest gameState
            where
                Move x y player = move
                currentPlayer = otherPlayer player
                updatedBoard = updateBoard currentBoard move
                oldEval = evaluateIntersection currentBoard (x,y)
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
    (filter (\move -> (fives $ playerEval $ evaluateIntersectionForMove board move) > 0) neighbouringMoves)
    where
        (GameState board _ _) = gameState
        playerEval = case player of
            Black -> black
            White -> white
        neighbouringMoves = genNeighboringMoves board 1 player

nonLosingMoves :: GameState -> Player -> [Move]
nonLosingMoves gameState player =
    map (\(Move x y _) -> Move x y player) $ winningMoves gameState $ otherPlayer player


winningInNextTurnMoves :: GameState -> Player -> [Move]
winningInNextTurnMoves gameState player =
    (filter (\move -> (straightFours $ playerEval $ evaluateIntersectionForMove board move) >= 1) neighbouringOneField)
    ++
    (filter (\move -> (fours $ playerEval $ evaluateIntersectionForMove board move) >= 2) neighbouringTwoFields)
    where
        (GameState board _ _) = gameState
        playerEval = case player of
            Black -> black
            White -> white
        neighbouringOneField = genNeighboringMoves board 1 player
        neighbouringTwoFields = genNeighboringMoves board 2 player

nonLosingInNextTurnMoves :: GameState -> Player -> [Move]
nonLosingInNextTurnMoves gameState player =
    map (\(Move x y _) -> Move x y player) $ winningInNextTurnMoves gameState $ otherPlayer player

bestMoves :: BitBoard -> Player -> Int -> [(Int, Move)]
bestMoves board player numBest =
    L.take numBest sortedMovesWithScore
    where
        allNextMoves = genNeighboringMoves board 2 player
        movesWithScore = L.map (\m -> (evalMove m, m)) allNextMoves
        sortedMovesWithScore = L.reverse $ L.sort movesWithScore
        evalMove move@(Move x y _) =
            (score $ (playerScore player) $ evaluateIntersectionForMove board move) +
            (score $ (playerScore $ otherPlayer player) $ evaluateIntersectionForMove board enemyMove)
            where
                enemyMove = Move x y $ otherPlayer player