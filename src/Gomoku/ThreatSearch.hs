module Gomoku.ThreatSearch where

import Gomoku.Abstractions
import Gomoku.AI
import Gomoku.BitBoardImpl
import Gomoku.GameState
import Gomoku.Patterns

import qualified Data.List as L
import Data.Tree
import Data.Maybe
import qualified Data.Set as S

import Debug.Trace

threats :: GameState -> Player -> [Move]
threats game  player =
    (filter (\move -> (fours $ playerEval $ evaluateIntersectionForMove bitboard move) == 1) neighbouringTwoFields)
    ++
    (filter (\move -> (openThrees $ playerEval $ evaluateIntersectionForMove bitboard move) == 1) neighbouringTwoFields)
    ++
    (filter (\move -> (brokenThrees $ playerEval $ evaluateIntersectionForMove bitboard move) == 1) neighbouringTwoFields)
    where
        GameState bitboard _ _ = game
        playerEval = case player of
            Black -> black
            White -> white
        neighbouringOneField = genNeighboringMoves bitboard 1 player
        neighbouringTwoFields = genNeighboringMoves bitboard 2 player

threatsTree :: GameState -> Tree GameState
threatsTree game =
    Node {
        rootLabel = game,
        subForest = if gameIsOver $ evaluation game
            then []
            else L.map (nextGameState) nextMoves'
    }
    where
        GameState currentBoard bEval ((Move _ _ lastPlayer):_) = game
        currentPlayer = otherPlayer lastPlayer

        nextMoves' =
            case listToMaybe (winningMoves game currentPlayer) of
                Just m -> [m]
                Nothing ->
                    case listToMaybe (nonLosingMoves game currentPlayer) of
                        Just m -> [m]
                        Nothing ->
                            case listToMaybe (winningInNextTurnMoves game currentPlayer) of
                                Just m -> [m]
                                Nothing -> otherMoves

        otherMoves = rmdups $ (threats game currentPlayer) ++ (nonLosingInNextTurnMoves game currentPlayer)


        nextGameState :: Move -> Tree GameState
        nextGameState move =
            threatsTree gameState
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

winningThreatSequence :: GameState -> Int -> [Move]
winningThreatSequence game depth =
    L.map (getLastMove) winningSequences
    where
        GameState _ _ ((Move _ _ lastPlayer):_) = game
        player = otherPlayer lastPlayer
        tree = threatsTree game
        candidates = subForest tree
        winningSequences = filter (\candidateTree -> allWinning depth candidateTree player) candidates
        getLastMove = head.moves.rootLabel
        firstWinningSequence = head winningSequences

allWinning 0 tree player =
    if L.null $ subForest tree
        then winningBranch player (rootLabel tree)
        else L.all ((winningBranch player).rootLabel) $ subForest tree

allWinning n tree player =
    if L.null $ subForest tree
        then winningBranch player (rootLabel tree)
        else L.all (\branchGame -> anyWinning (n - 1) branchGame player) $ subForest tree

anyWinning 0 tree player =
    if L.null $ subForest tree
        then winningBranch player (rootLabel tree)
        else L.any ((winningBranch player).rootLabel) $ subForest tree

anyWinning n tree player =
    if L.null $ subForest tree
        then winningBranch player (rootLabel tree)
        else L.any (\branchGame -> allWinning (n - 1) branchGame player) $ subForest tree


winningBranch :: Player -> GameState -> Bool
winningBranch player game =
    (fives pScore > 0)
    where
        pScore = (playerScore player) $ evaluation game
        pThreats = (fours pScore) + (openThrees pScore) + (brokenThrees pScore)


printThreatsTree :: Tree GameState -> IO ()
printThreatsTree tree =
    putStrLn $ drawTree $ fmap (show) tree


printThreatSearchResults gameState =
    putStrLn $
    L.concat $ L.intersperse "\n" $
    fmap (\(n, s) -> (show n) ++ "\t" ++ (show s)) [(n, winningThreatSequence gameState n) | n <- [0..20]]

----------
-- Helpers
----------

rmdups :: Ord a => [a] -> [a]
rmdups = S.toList . S.fromList