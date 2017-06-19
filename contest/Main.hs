module Main where

import System.Environment
import System.IO

import Gomoku.Abstractions
import Gomoku.AI
import Gomoku.GameState
import Gomoku.InputParser
import Gomoku.ThreatSearch

import Data.Maybe

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin NoBuffering
    args <- getArgs
    let firstArg = head args
    let player = case firstArg of
            "b" -> Black
            "w" -> White
            _   -> error "invalid argument, allowed: 'b' or 'w'"
    gameState <- case player of
            Black -> do
                printMove $ Move 9 9 Black
                return $ createGameState 19 [Move 9 9 Black]
            White -> do
                return $ createGameState 19 []
    loop gameState (otherPlayer player)
    return ()

loop :: GameState -> Player -> IO ()
loop gameState enemyPlayer = do
    enemyMove <- externalAgent enemyPlayer
    let gameStateAfterEnemysMove = updateGameState gameState enemyMove
    if gameIsOver $ evaluation gameStateAfterEnemysMove
        then do
            putStrLn "Lost"
            return ()
        else do
            let myMove = aiAgent gameStateAfterEnemysMove
            printMove myMove
            let gameStateAfterMyMove = updateGameState gameStateAfterEnemysMove myMove
            if gameIsOver $ evaluation gameStateAfterMyMove
                then do
                    putStrLn "Won"
                    return ()
                else
                    loop gameStateAfterMyMove enemyPlayer

externalAgent :: Player -> IO Move
externalAgent player = do
    input <- getLine
    return $ parseMove player input

aiAgent :: GameState -> Move
aiAgent gameState =
    case sureWin of
        Just move -> move
        Nothing -> minimax (movesTreeOnlyBest 8) 4 gameState
    where
        sureWin = listToMaybe $ winningThreatSequence gameState 8

printMove :: Move -> IO ()
printMove (Move x y _) =
    putStrLn $ "(" ++ show (x + 1) ++ "," ++ show (y + 1) ++ ")"
