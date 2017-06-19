module Main (
    main
) where

import Gomoku.Abstractions
import Gomoku.AI
import Gomoku.GameState
import Gomoku.InputParser
import Gomoku.ThreatSearch

import Data.Maybe


main :: IO ()
main = humanMove newGame Black

humanMove :: GameState -> Player -> IO ()
humanMove gameState player = do
    putStrLn $ show $ board gameState
    putStrLn ("Player " ++ show player)
    line <- getLine
    let playerMove = parseMove player line
    let newGameState = updateGameState gameState playerMove
    continueGame newGameState player aiMove

aiMove :: GameState -> Player -> IO ()
aiMove gameState player = do
    putStrLn $ show $ board gameState
    putStrLn ("Player " ++ show player)
    let move = aiAgent gameState
    let newGameState = updateGameState gameState move
    return ()
    continueGame newGameState player humanMove

continueGame :: GameState -> Player -> (GameState -> Player -> IO ()) -> IO ()
continueGame gameState player nextMoveFun = do
    if gameIsOver $ evaluation gameState
    then do
        putStrLn "Game Over"
        return ()
    else do
        nextMoveFun gameState $ otherPlayer player

aiAgent :: GameState -> Move
aiAgent gameState =
    case sureWin of
        Just move -> move
        Nothing -> minimax (movesTreeOnlyBest 8) 4 gameState
    where
        sureWin = listToMaybe $ winningThreatSequence gameState 8